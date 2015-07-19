package com.rethinkscala.backend.netty


import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong}

import com.rethinkscala.backend.Connection
import com.rethinkscala.net.{Version, VersionHandler}
import com.rethinkscala.utils.ConnectionWithId
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

  protected[rethinkscala] val pool = new ConnectionPool(NettyConnectionFactory(version, bootstrap, connectionId), max = version.maxConnections)


  def write[T](term: Term, opts: Map[String, Any], connectionId: Option[Long] = None)(implicit extractor: ResultExtractor[T]): Promise[T] = {
    val p = Promise[T]()
    val f = p.future
    logger.debug(s"Writing $term")
    pool.take(connectionId, Some(term.toString)) {
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
            logger.debug("Writing query Token = " + query.tokenId)
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




abstract class ForwardingConnection(val underlying: Connection) extends Connection {
  override val version: Version = underlying.version

  override def write[T](term: Term, opts: Map[String, Any], connectionId: Option[Long])(implicit extractor: ResultExtractor[T]) =
    underlying.write[T](term, opts, connectionId)(extractor)

  override protected[rethinkscala] val pool: ConnectionPool = underlying.pool
}


