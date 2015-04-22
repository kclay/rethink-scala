package com.rethinkscala.net


import java.net.InetSocketAddress
import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong}

import com.rethinkscala.ast._
import com.rethinkscala.utils.{RethinkConnectionPool, ConnectionFactory, ConnectionWithId, SimpleConnectionPool}
import com.rethinkscala.{ResultExtractor, Term}
import com.typesafe.scalalogging.slf4j.LazyLogging
import io.netty.bootstrap.Bootstrap
import io.netty.channel._
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.nio.NioSocketChannel
import ql2.{Ql2 => ql2}

import scala.concurrent.duration.Duration
import scala.concurrent.{Future, Promise}


/** Created by IntelliJ IDEA.
  * User: Keyston
  * Date: 3/23/13
  * Time: 2:53 PM
  */


/*
case class SingleConnection(version: Version) extends Connection {
  override val underlying: Connection = this

  override def write[T](term: Term, opts: Map[String, Any])(implicit mf: Manifest[T]) = ???
} */

class ConnectionAttachment[T](versionHandler: VersionHandler[T], restore: Throwable => Unit) {
  def handle(tokenId: Long, response: T) = versionHandler.handle(tokenId, response)

  def failure(e: Throwable) = {
    versionHandler.failure(e)
    restore(e)
  }

  override def toString = {
    s"ConnectionAttachment($versionHandler)"
  }
}

case class ConnectionChannel(cf: ChannelFuture, id: Long) extends ConnectionWithId {
  type Id = Int

  val token: AtomicLong = new AtomicLong()

  @volatile
  var invalidated = false

  @volatile
  @volatile
  var configured: Boolean = false
  lazy val channel = cf.channel()
  @volatile
  override var active: AtomicBoolean = new AtomicBoolean(false)
}

abstract class AbstractConnection(val version: Version) extends LazyLogging with Connection {

  private val defaultDB = Some(version.db.getOrElse("test"))
  private[this] val connectionId = new AtomicLong()

  implicit val exc = version.executionContext


  lazy val bootstrap = {

    // Configure the client.
    val group = new NioEventLoopGroup()


    val b = new Bootstrap()

    version.connectTimeout.foreach(timeout => b.option(ChannelOption.CONNECT_TIMEOUT_MILLIS.asInstanceOf[ChannelOption[Any]], timeout))
    b.group(group)
      .channel(classOf[NioSocketChannel])
      .handler(version.channelInitializer)


      /**
       * Error:(83, 29) type mismatch;
       found   : io.netty.channel.ChannelOption[Boolean]
       required: io.netty.channel.ChannelOption[Any]
      Note: Boolean <: Any, but Java-defined class ChannelOption is invariant in type T.
      You may wish to investigate a wildcard type such as `_ <: Any`. (SLS 3.2.10)
            .option(ChannelOption.TCP_NODELAY, true)

       */
      .option(ChannelOption.TCP_NODELAY.asInstanceOf[ChannelOption[Any]], true) // fixed an scala compiler bug


  }


  lazy val versionHandler = version.newHandler

  private[rethinkscala] def get(connectionId: Int)(implicit timeout: Duration): Future[Connection] = ???

  protected[rethinkscala] val pool = new RethinkConnectionPool(new ConnectionFactory[ConnectionChannel] {

    def create(): ConnectionChannel = {

      logger.debug("Creating new ChannelWrapper")
      val c = bootstrap.connect(new InetSocketAddress(version.host, version.port)).sync()

      val cf = ChannelAttribute.Future.get(c.channel())

      new ConnectionChannel(cf, connectionId.incrementAndGet())
    }

    def configure(wrapper: ConnectionChannel) = {
      wrapper.active.set(true)
      if (!wrapper.configured) {
        logger.debug("Configuring ChannelWrapper")
        //  version.configure(wrapper.channel)
        wrapper.configured = true
      } else {
        logger.debug("Logger already configured")
      }
    }

    def validate(wrapper: ConnectionChannel): Boolean = {
      wrapper.channel.isOpen
    }

    def destroy(wrapper: ConnectionChannel) {
      logger.debug("Destroying Channel")
      wrapper.invalidated = true
      wrapper.channel.close()
    }
  }, max = version.maxConnections)


  def write[T](term: Term, opts: Map[String, Any], connectionId: Option[Long] = None)(implicit extractor: ResultExtractor[T]): Promise[T] = {
    val p = Promise[T]()
    val f = p.future
    logger.debug(s"Writing $term")
    pool.take(connectionId) {
      case (c, restore, invalidate) =>
        logger.debug("Received connection from pool")
        val con = this
        // add a channel future to ensure that all setup has been done
        c.cf.addListener(new ChannelFutureListener {
          def operationComplete(future: ChannelFuture) {
            logger.debug("Channel Future completed")

            logger.debug("Creating query")
            val query = versionHandler.newQuery(con, c.id, term, p, None, opts)

            // TODO : Check into dropping netty and using sockets for each,

            val attachment = new ConnectionAttachment(versionHandler, e => {
              logger.debug(s"Invalidating connection (${c.id})")
              invalidate(c)
              p.tryFailure(e)
            })

            logger.debug(s"Creating connection attachment ")
            // Or find a way so that we can store the token for the netty handler to complete
            ChannelAttribute.Handler.set(c.channel, attachment)
            logger.debug("Writing query")
            c.channel.writeAndFlush(query)
            logger.debug("Wrote query")
            future.removeListener(this)

          }
        })
        f onComplete {
          case _ => if (!c.invalidated) {
            logger.debug(s"Restoring connection (${c.id})")
            restore(c)
          }
        }

    } onFailure {
      case e: Exception => p.tryFailure(e)
    }
    p
  }
}

trait Connection {
  val underlying: Connection
  val version: Version
  protected[rethinkscala] val pool: RethinkConnectionPool

  def toAst(term: Term): CompiledAst = version.toAst(term)

  def write[T](term: Term, opts: Map[String, Any], connectionId: Option[Long] = None)(implicit extractor: ResultExtractor[T]): Promise[T]
}


trait ConnectionOps[C <: Connection, D <: Mode[C]] {
  self: C =>


  val delegate: D


  def newQuery[R](term: Term, extractor: ResultExtractor[R], opts: Map[String, Any]): ResultQuery[R]

}


trait BlockingConnection extends Connection with ConnectionOps[BlockingConnection, Blocking] {


  val delegate = Blocking

  // FIXME : Need to place here to help out Intellijd
  def apply[T](produce: Produce[T])(implicit extractor: ResultExtractor[T]) = delegate(produce)(this).run

  def toOpt[T](produce: Produce[T])(implicit extractor: ResultExtractor[T]) = delegate(produce)(this).toOpt

  val timeoutDuration: Duration
}

trait AsyncConnection extends Connection with ConnectionOps[AsyncConnection, Async] {

  val delegate = Async

  // FIXME : Need to place here to help out Intellij with async(_.apply(res))
  def apply[T](produce: Produce[T])(implicit extractor: ResultExtractor[T]) = delegate(produce)(this).run

  def toOpt[T](produce: Produce[T])(implicit extractor: ResultExtractor[T]) = delegate(produce)(this).toOpt

}

object AsyncConnection {

  def apply(version: Version) = build(version, None)


  def apply(connection: Connection) = connection match {
    case c: AsyncConnection => c
    case c: BlockingConnection => build(connection.version, Some(connection))
  }


  private def build(v: Version, under: Option[Connection]) = new AbstractConnection(v) with AsyncConnection {
    val underlying: Connection = under.getOrElse(this)
    override val version = v

    def newQuery[R](term: Term, extractor: ResultExtractor[R], opts: Map[String, Any]) = AsyncResultQuery[R](term, this, extractor, opts)

  }
}

private class ConnectionWithAsync(v: Version, under: Option[Connection]) extends AbstractConnection(v) with AsyncConnection {
  val underlying: Connection = under.getOrElse(this)
  override val version = v

  def newQuery[R](term: Term, extractor: ResultExtractor[R], opts: Map[String, Any]) = AsyncResultQuery[R](term, this, extractor, opts)
}

object BlockingConnection {
  val defaultTimeoutDuration = Duration(30, "seconds")


  def apply(connection: Connection) = connection match {
    case c: BlockingConnection => c
    case c: AsyncConnection => build(connection.version, defaultTimeoutDuration, Some(connection))
  }

  def apply(version: Version, timeoutDuration: Duration = defaultTimeoutDuration) = build(version, timeoutDuration, None)

  private def build(v: Version, t: Duration, under: Option[Connection]) = new AbstractConnection(v) with BlockingConnection {
    val timeoutDuration = t
    val underlying: Connection = under.getOrElse(this)
    override val version = v

    def newQuery[R](term: Term, extractor: ResultExtractor[R], opts: Map[String, Any]) = BlockingResultQuery[R](term, this, extractor, opts)
  }
}



