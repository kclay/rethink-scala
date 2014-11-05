package com.rethinkscala.utils

import java.util.concurrent.ArrayBlockingQueue
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.TimeUnit
import com.typesafe.scalalogging.slf4j.LazyLogging

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.Duration

/** Created by IntelliJ IDEA.
  * User: Keyston
  * Date: 3/24/13
  * Time: 8:33 PM
  *
  */

//https://github.com/jamesgolick/scala-connection-pool/
trait ConnectionFactory[Connection] {
  def create(): Connection

  def validate(connection: Connection): Boolean

  def configure(connection: Connection): Unit

  def destroy(connection: Connection): Unit
}

trait ConnectionPool[Connection] {
  def apply[A]()(f: Connection => A): A
}


trait ConnectionWithId {

  type Id
  val id: Id
}

trait LowLevelConnectionPool[Connection] {
  def borrow(): Connection

  def giveBack(conn: Connection): Unit

  def invalidate(conn: Connection): Unit
}

class TimeoutError(message: String) extends Error(message)

class SimpleConnectionPool[Conn <: ConnectionWithId](connectionFactory: ConnectionFactory[Conn],
                                                     max: Int = 20,
                                                     timeout: Int = 500000)


  extends ConnectionPool[Conn] with LowLevelConnectionPool[Conn] with LazyLogging {


  private val size = new AtomicInteger(0)

  private val pool = new ArrayBlockingQueue[Conn](max)

  private val connections = new com.google.common.collect.MapMaker()
    .concurrencyLevel(4)
    .weakKeys()
    .makeMap[Conn#Id, Conn]

  private val pending = new com.google.common.collect.MapMaker()
    .concurrencyLevel(4)
    .weakKeys()
    .makeMap[Conn#Id, Future[Conn]]


  def getById(id: Int)(implicit timeout: Duration): Future[Conn] = ???

  def apply[A]()(f: Conn => A): A = {
    val connection = borrow()


    logger.debug(s"Borrowed connection with id = ${connection.id}")

    try {
      val result = f(connection)
      giveBack(connection)
      result
    } catch {
      case t: Throwable =>
        invalidate(connection)
        throw t
    }
  }

  def take(block: (Conn, Conn => Unit, Conn => Unit) => Unit)(implicit exc: ExecutionContext): Future[Unit] = {

    val connection = borrow()
    logger.debug(s"take connection with id (${connection.id})")
    connections.putIfAbsent(connection.id, connection)


    val f = Future {

      connectionFactory.configure(connection)
      block(connection, giveBack, invalidate)


    }

    f onFailure {
      case t: Throwable =>
        invalidate(connection)


    }
    f
  }

  def total = size.get()

  def nonEmpty = size.get() > 0

  def isEmpty = size.get() == 1

  def borrow(): Conn = Option(pool.poll()).getOrElse(createOrBlock)


  def available = pool.size()

  def giveBack(connection: Conn): Unit = {
    logger.debug(s"giveBack(connection:${connection.id})")
    pool.offer(connection)
  }

  def invalidate(connection: Conn): Unit = {
    logger.debug(s"invalidate(connection:${connection.id})")
    connectionFactory.destroy(connection)
    connections.remove(connection.id, connection)
    size.decrementAndGet
  }

  private def createOrBlock: Conn = {
    val amount = size.get
    logger.debug(s"createOrBlock size=$amount max = $max")
    amount match {
      case e: Int if e == max => block
      case _ => create
    }
  }

  private def create: Conn = {
    size.incrementAndGet match {
      case e: Int if e > max =>
        logger.debug(s"create $e > $max")

        size.decrementAndGet
        borrow()
      case e: Int => connectionFactory.create()


    }
  }

  private def block: Conn = {
    Option(pool.poll(timeout, TimeUnit.NANOSECONDS)) getOrElse {
      throw new TimeoutError("Couldn't acquire a connection in %d nanoseconds.".format(timeout))
    }
  }
}