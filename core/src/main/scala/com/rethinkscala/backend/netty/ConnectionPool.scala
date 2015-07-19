package com.rethinkscala.backend.netty

import java.util.concurrent.{TimeUnit, LinkedBlockingDeque, ArrayBlockingQueue}
import java.util.concurrent.atomic.AtomicInteger

import com.rethinkscala.utils.{TimeoutError, LowLevelConnectionPool, ConnectionFactory, AbstractConnectionPool}
import com.typesafe.scalalogging.slf4j.LazyLogging

import scala.collection.concurrent.TrieMap
import scala.concurrent.{Promise, ExecutionContext, Future}
import scala.util.Try

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 7/17/2015
 * Time: 6:13 PM 
 */

class ConnectionPool(connectionFactory: ConnectionFactory[ConnectionChannel],
                     max: Int = 20,
                     timeout: Int = 5000)

  extends AbstractConnectionPool[ConnectionChannel] with LowLevelConnectionPool[ConnectionChannel] with LazyLogging {


  type Conn = ConnectionChannel
  private val size = new AtomicInteger(0)

  private val pool = new ArrayBlockingQueue[Conn](max)

  private val connections = new com.google.common.collect.MapMaker()
    .concurrencyLevel(4)
    .weakKeys()
    .makeMap[Long, Conn]


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


  def getConnectionById(id: Long): Option[Conn] = Option(connections.get(id))

  trait ScopedPromised {
    val tag: Option[String]
    val future: Future[Conn]

    def trySuccess(value: Conn): Unit

    def onComplete[U](f: Try[Conn] => U): Unit
  }


  private val pending: TrieMap[Long, LinkedBlockingDeque[ScopedPromised]] = TrieMap.empty

  def take(connectionId: Option[Long], name: Option[String] = None)(block: (Conn, Conn => Unit, Conn => Unit) => Unit)(implicit exc: ExecutionContext): Future[Conn] = {

    def execute(connection: Conn): Future[Conn] = Future {

      connections.putIfAbsent(connection.id, connection)
      connectionFactory.configure(connection)
      logger.debug(s"Entering executing block for (${connection.id}})")
      block(connection, giveBack, invalidate)
      logger.debug(s"Exiting executing block for (${connection.id}")
      connection

    }

    connectionId.fold({
      logger.debug("No connectionId executing")
      execute(borrow())
    })(id => {
      val maybe = getConnectionById(id)
      maybe match {
        case Some(c) if !c.active.get() =>
          logger.debug("Connection found and not active")
          execute(c)

        case _ =>
          val p = Promise[Conn]()
          if (maybe.isDefined) logger.debug(s"Connection ($id) found but currently active")
          else logger.debug(s"No connection found for ($id) so creating promise")


          val scoped = new ScopedPromised {
            val tag = name
            override val future: Future[Conn] = p.future.flatMap(conn => {
              logger.debug(s"Promise resolved for ($id)  executing name={$tag}")
              execute(conn)
            })

            override def trySuccess(value: Conn) = p.trySuccess(value)

            override def onComplete[U](f: (Try[Conn]) => U) = future.onComplete(f)
          }

          pending
            .getOrElseUpdate(1, new LinkedBlockingDeque[ScopedPromised]())
            .add(scoped)

          logger.debug(s"Connection ($id) now has ${pending(id).size} pending queries")

          p.future


      }
    })


  }

  def total = size.get()

  def nonEmpty = size.get() > 0

  def isEmpty = size.get() == 1

  def borrow(): Conn = Option(pool.poll()).getOrElse(createOrBlock)


  def available = pool.size()

  private[this] val emptyConnectionQueue = new LinkedBlockingDeque[ScopedPromised]()

  def giveBack(connection: Conn): Unit = {
    logger.debug(s"giveBack(connection:${
      connection.id
    })")


    val hasPending = pending.getOrElse(connection.id, emptyConnectionQueue)
    def drain(): Unit = if (!hasPending.isEmpty) {
      logger.debug(s"drain(connection:${connection.id}, pending : ${hasPending.size}})")

      val current = hasPending.remove()
      logger.debug(s"Removing promise nam=${current.tag}")
      current.onComplete {
        case _ =>
          logger.debug(s"Processed ScopedPromised for (${connection.id})")
          drain()
      }
      current.trySuccess(connection)
    } else {
      logger.debug(s"drain(connection:${connection.id}) empty ")
      connection.active.compareAndSet(true, false)
      pool.offer(connection)

    }
    drain()

  }

  def invalidate(connection: Conn): Unit = {
    logger.debug(s"invalidate(connection:${
      connection.id
    }) total = ${
      size.get()
    }")
    connectionFactory.destroy(connection)
    connections.remove(connection.id, connection)
    // TODO : Some how resovle Query.CONTINUE results
    size.decrementAndGet
    logger.debug(s"Finished invalidating(connect:${
      connection.id
    } total = ${
      size.get
    }")
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