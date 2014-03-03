package com.rethinkscala.net


import org.jboss.netty.channel.socket.nio.NioClientSocketChannelFactory
import java.util.concurrent.Executors
import org.jboss.netty.bootstrap.ClientBootstrap
import java.net.InetSocketAddress

import org.jboss.netty.channel._

import ql2.{Ql2 => ql2}
import ql2.{Query, Response, VersionDummy}

import org.jboss.netty.handler.codec.oneone.OneToOneEncoder
import org.jboss.netty.buffer.ChannelBuffers._
import org.jboss.netty.buffer.{ChannelBuffer, HeapChannelBufferFactory}
import java.nio.ByteOrder
import com.rethinkscala.utils.{ConnectionFactory, SimpleConnectionPool}
import concurrent._
import org.jboss.netty.channel.Channels.pipeline
import com.rethinkscala.ConvertFrom._

import org.jboss.netty.channel.Channel
import ql2.Response.ResponseType
import com.rethinkscala.ast._
import org.jboss.netty.handler.codec.frame.FrameDecoder
import java.util.concurrent.atomic.AtomicInteger

import Translate._
import com.rethinkscala.Term
import scala.concurrent.duration.Duration
import com.typesafe.scalalogging.slf4j.Logging
import scala.Some
import com.rethinkscala.ast.DB


/** Created by IntelliJ IDEA.
  * User: Keyston
  * Date: 3/23/13
  * Time: 2:53 PM
  */


abstract class Token {
  type ResultType
  val query: ql2.Query
  val term: Term

  def handle(response: Response)


}


case class QueryToken[R](connection: Connection, query: ql2.Query, term: Term, p: Promise[R], mf: Manifest[R]) extends Token with Logging {


  implicit val t = mf

  private[rethinkscala] def context = connection

  type ResultType = R
  type MapType = Map[String, _]
  type IterableType = Iterable[MapType]

  /*def cast(value: Any, json: String): ResultType = value match {
    case v: MapType => translate[MapType, ResultType].read(v.asInstanceOf[MapType], json, term)
    case a: IterableType => translate[IterableType, ResultType].read(a.asInstanceOf[IterableType], json, term)
    case x: Any => x.asInstanceOf[R]

  }        */

  def cast[T](json: String)(implicit mf: Manifest[T]): T = translate[T].read(json, term)


  def toResult(response: Response) = {
    logger.debug(s"Processing result : $response")
    val json: String = Datum.unwrap(response.getResponse(0))

    val rtn = json match {
      case "" => None

      case _ => cast[ResultType](json)
    }


    rtn


  }

  /*
  def toResult(response: Response) = {
    val (data: Any, json: String) = Datum.unwrap(response.getResponse(0))

    val rtn = data match {
      case None => None
      case _ => cast(json)
    }


    rtn


  }    */

  def handle(response: Response) = (response.getType match {

    case ResponseType.RUNTIME_ERROR | ResponseType.COMPILE_ERROR | ResponseType.CLIENT_ERROR => toError(response, term)
    case ResponseType.SUCCESS_PARTIAL | ResponseType.SUCCESS_SEQUENCE => toCursor(0, response)
    case ResponseType.SUCCESS_ATOM => term match {
      case x: ProduceSequence[_] => toCursor(0, response)
      case _ => toResult(response)
    }
    // case ResponseType.SUCCESS_ATOM => toResult(response)
    case _ =>

  }) match {
    case e: Exception => p failure e
    case e: Any => p success e.asInstanceOf[R]
  }


  def toCursor(id: Int, response: Response) = {
    import scala.collection.JavaConverters._

    //val seqManifest = implicitly[Manifest[Seq[R]]]


    val results = response.getResponseList.asScala
    val seq = results.length match {
      case 1 if response.getType == ResponseType.SUCCESS_ATOM =>
       cast[ResultType](Datum.unwrap(results(0)))
      case _ => for (d <- results) yield Datum.unwrap(d) match {

        case json: String => if (mf.typeArguments.nonEmpty) cast(json)(mf.typeArguments(0)) else cast[ResultType](json)
      }

    }

    new Cursor[R](id, this, seq.asInstanceOf[Seq[R]], response.getType match {
      case ResponseType.SUCCESS_SEQUENCE => true
      case _ => false
    })

  }


}


class RethinkDBHandler extends SimpleChannelUpstreamHandler {

  implicit def channelHandlerContext2Promise(ctx: ChannelHandlerContext): Option[Token] = Some(ctx.getChannel.getAttachment.asInstanceOf[Token])


  override def messageReceived(ctx: ChannelHandlerContext, e: MessageEvent) {

    ctx.map(_.handle(e.getMessage.asInstanceOf[Response]))


  }

  override def exceptionCaught(ctx: ChannelHandlerContext, e: ExceptionEvent) {

    // ctx.map(_.failure(e.getCause))
  }
}

private class RethinkDBFrameDecoder extends FrameDecoder {
  def decode(ctx: ChannelHandlerContext, channel: Channel, buffer: ChannelBuffer): AnyRef = {
    buffer.markReaderIndex




    if (!buffer.readable() || buffer.readableBytes() < 4) {
      buffer.resetReaderIndex()
      return null
    }

    val length = buffer.readInt()

    if (buffer.readableBytes() < length) {
      buffer.resetReaderIndex()
      return null
    }
    return buffer.readBytes(length)

  }
}

private class RethinkDBEncoder extends OneToOneEncoder {

  def encode(ctx: ChannelHandlerContext, channel: Channel, msg: Any): AnyRef = {

    msg match {
      case v: VersionDummy.Version => {
        val b = buffer(ByteOrder.LITTLE_ENDIAN, 4)
        b.writeInt(v.getNumber)
        b
      }
      case s: String => {
        val b = buffer(ByteOrder.LITTLE_ENDIAN, s.length + 4)
        b.writeInt(s.length)
        b.writeBytes(s.getBytes("ascii"))
        b
      }
      case q: ql2.Query => {
        val size = q.getSerializedSize
        val b = buffer(ByteOrder.LITTLE_ENDIAN, size + 4)
        b.writeInt(size)

        b.writeBytes(q.toByteArray)
        b
      }
    }

  }
}

private class PipelineFactory extends ChannelPipelineFactory {
  // stateless
  val defaultHandler = new RethinkDBHandler()


  def getPipeline: ChannelPipeline = {
    val p = pipeline()


    p.addLast("frameDecoder", new RethinkDBFrameDecoder())
    p.addLast("protobufDecoder", new ProtobufDecoder2(Response.getDefaultInstance))

    //p.addLast("frameEncoder", new ProtobufVarint32LengthFieldPrepender());

    p.addLast("protobufEncoder", new RethinkDBEncoder())

    p.addLast("handler", defaultHandler)
    p
  }
}


abstract class AbstractConnection(version: Version) extends Logging with Connection {

  private val defaultDB = Some(version.db.getOrElse("test"))
  private[this] val connectionId = new AtomicInteger()

  implicit val exc = version.executionContext


  lazy val bootstrap = {

    val factory =
      new NioClientSocketChannelFactory(
        Executors.newCachedThreadPool(),
        Executors.newCachedThreadPool())

    val b = new ClientBootstrap(factory)
    b.setPipelineFactory(new PipelineFactory())
    b.setOption("tcpNoDelay", true)
    b.setOption("keepAlive", true)
    b.setOption("bufferFactory", new HeapChannelBufferFactory(ByteOrder.LITTLE_ENDIAN))
    b

  }


  case class ChannelWrapper(cf: ChannelFuture) {
    val id = connectionId.incrementAndGet()
    val token: AtomicInteger = new AtomicInteger()
    @volatile
    var configured: Boolean = false
    lazy val channel = cf.getChannel
  }


  protected[rethinkscala] val channel = new SimpleConnectionPool(new ConnectionFactory[ChannelWrapper] {

    def create(): ChannelWrapper = {

      logger.debug("Creating new ChannelWrapper")
      val c = bootstrap.connect(new InetSocketAddress(version.host, version.port)).await()

      new ChannelWrapper(c)
    }

    def configure(wrapper: ChannelWrapper) = {
      if (!wrapper.configured) {
        logger.debug("Configuring ChannelWrapper")
        version.configure(wrapper.channel)
        wrapper.configured = true
      } else {
        logger.debug("Logger already configured")
      }
    }

    def validate(wrapper: ChannelWrapper): Boolean = {

      wrapper.channel.isOpen
    }

    def destroy(wrapper: ChannelWrapper) {
      logger.debug("Destroying Channel")
      wrapper.channel.close()
    }
  }, max = version.maxConnections)

  private def toQuery(term: Term, token: Int, db: Option[String] = None, opts: Map[String, Any] = Map()) = {

    def scopeDB(q: Query.Builder, db: DB) = q.addGlobalOptargs(Query.AssocPair.newBuilder.setKey("db").setVal(db.ast))

    val query = Some(
      Query.newBuilder().setType(Query.QueryType.START)
        .setQuery(term.ast).setToken(token).setAcceptsRJson(true)

    ).map(q => {

      opts.get("db").map {
        case name: String => scopeDB(q, DB(name))
      }.getOrElse {
        term match {
          case d: WithDB => d.db.map(scopeDB(q, _)).getOrElse(db.map {
            name => scopeDB(q, DB(name))
          }.getOrElse(q))
          case _ => db.map {
            name => scopeDB(q, DB(name))
          }.getOrElse(q)
        }

      }


    }).get

    query.build()

  }


  def write[T](term: Term, opts: Map[String, Any])(implicit mf: Manifest[T]): Promise[T] = {
    val p = promise[T]()
    val f = p.future
    logger.debug(s"Writing $term")
    channel take {
      case (c, restore) =>

        logger.debug("Received connection from pool")
        val con = this
        // add a channel future to ensure that all setup has been done
        c.cf.addListener(new ChannelFutureListener {
          def operationComplete(future: ChannelFuture) {

            val query = toQuery(term, c.token.getAndIncrement, defaultDB, opts)


            val token = QueryToken[T](con, query, term, p, mf)
            // TODO : Check into dropping netty and using sockets for each,
            // Or find a way so that we can store the token for the netty handler to complete

            c.channel.setAttachment(token)
            logger.debug("Writing query")
            c.channel.write(query)
            future.removeListener(this)
          }
        })


        f onComplete {
          case _ => restore(c)
        }
    } onFailure {
      case e: Exception => p.failure(e)
    }

    p
  }
}

trait Connection {
  val underlying: Connection
  val version: Version

  def write[T](term: Term, opts: Map[String, Any])(implicit mf: Manifest[T]): Promise[T]
}

trait QueryFactory {
  def newQuery[R](term: Term, mf: Manifest[R], opts: Map[String, Any]): ResultQuery[R]
}

trait BlockingConnection extends Connection with QueryFactory {

  import com.rethinkscala.Blocking

  val delegate: Blocking.type = Blocking
  val timeoutDuration: Duration
}

trait AsyncConnection extends Connection with QueryFactory {

  import com.rethinkscala.Async

  val delegate: Async.type = Async
}

object AsyncConnection {
  def apply(version: Version) = build(version, None)


  def apply(connection: Connection) = connection match {
    case c: AsyncConnection => c
    case c: BlockingConnection => build(connection.version, Some(connection))
  }


  private def build(v: Version, under: Option[Connection]) = new AbstractConnection(v) with AsyncConnection {
    val underlying: Connection = under.getOrElse(this)
    val version = v

    def newQuery[R](term: Term, mf: Manifest[R], opts: Map[String, Any]) = AsyncResultQuery[R](term, this, mf, opts)

  }
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
    val version = v

    def newQuery[R](term: Term, mf: Manifest[R], opts: Map[String, Any]) = BlockingResultQuery[R](term, this, mf, opts)
  }
}



