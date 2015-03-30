package com.rethinkscala.utils

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{ArrayBlockingQueue, TimeUnit}

import com.typesafe.scalalogging.slf4j.LazyLogging

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.Duration
import scala.concurrent.{ExecutionContext, Future, Promise}

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


  val id: Long
  var active: Boolean
}

trait LowLevelConnectionPool[Connection] {
  def borrow(): Connection

  def giveBack(conn: Connection): Unit

  def invalidate(conn: Connection): Unit
}

class TimeoutError(message: String) extends Error(message)

class SimpleConnectionPool[Conn <: ConnectionWithId](connectionFactory: ConnectionFactory[Conn],
                                                     max: Int = 20,
                                                     timeout: Int = 5000)


  extends ConnectionPool[Conn] with LowLevelConnectionPool[Conn] with LazyLogging {


  private val size = new AtomicInteger(0)

  private val pool = new ArrayBlockingQueue[Conn](max)

  private val connections = new com.google.common.collect.MapMaker()
    .concurrencyLevel(4)
    .weakKeys()
    .makeMap[Long, Conn]


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

  private def checkout = {
    val connection = borrow()
    logger.debug(s"take connection with id (${connection.id})")
    connections.putIfAbsent(connection.id, connection)
    connection
  }

  case class ScopedPromised(promise: Promise[Conn], exc: ExecutionContext)

  private val pending: Map[Long, ArrayBuffer[ScopedPromised]] = Map.empty.withDefault(_ => ArrayBuffer.empty)

  def take(connectionId: Option[Long])(block: (Conn, Conn => Unit, Conn => Unit) => Unit)(implicit exc: ExecutionContext): Future[Conn] = {

    def execute(connection: Conn): Future[Conn] = Future {

      connections.putIfAbsent(connection.id, connection)
      connectionFactory.configure(connection)
      logger.debug(s"Entering executing block for (${connection.id}})")
      block(connection, giveBack, invalidate)
      logger.debug(s"Exiting executing block for (${connection.id}")
      connection

    }

    connectionId
      .fold({

      logger.debug("No connectionId executing")
      execute(borrow())
    }) {
      id =>
        val maybe = Option(connections.get(id))
        maybe match {
          case Some(c) if !c.active => {
            logger.debug("Connection found and not active")
            execute(c)
          }
          case _ =>
            val p = Promise[Conn]()
            if (maybe.isDefined) logger.debug(s"Connection ($id) found but currently active")
            else logger.debug(s"No connection found for ($id) so creating promise")

            p.future.onSuccess {
              case conn =>
                logger.debug("Promise resolved for ($id)  executing")
                execute(conn)
            }

            pending(id) += ScopedPromised(p, exc)

            logger.debug(s"Connection ($id) now has ${pending(id).size} pending queries")

            p.future


        }

    }


  }

  def total = size.get()

  def nonEmpty = size.get() > 0

  def isEmpty = size.get() == 1

  def borrow(): Conn = Option(pool.poll()).getOrElse(createOrBlock)


  def available = pool.size()

  def giveBack(connection: Conn): Unit = {
    logger.debug(s"giveBack(connection:${connection.id})")


    val hasPending = pending(connection.id)
    def drain(): Unit = if (hasPending.nonEmpty) {
      logger.debug(s"drain(connection:${connection.id}, pending : ${hasPending.size}})")

      val current = hasPending.remove(0)
      current.promise.future.onComplete {
        case _ => drain()
      }(current.exc)
      current.promise.trySuccess(connection)
    } else {
      logger.debug(s"drain(connection:${connection.id}) empty ")
      connection.active = false
      pool.offer(connection)
    }
    drain()

  }

  def invalidate(connection: Conn): Unit = {
    logger.debug(s"invalidate(connection:${connection.id}) total = ${size.get()}")
    connectionFactory.destroy(connection)
    connections.remove(connection.id, connection)
    // TODO : Some how resovle Query.CONTINUE results
    size.decrementAndGet
    logger.debug(s"Finished invalidating(connect:${connection.id} total = ${size.get}")
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
    Option(pool.poll(timeout, TimeUnit.MILLISECONDS)) getOrElse {
      throw new TimeoutError("Couldn't acquire a connection in %d nanoseconds.".format(timeout))
    }
  }
}