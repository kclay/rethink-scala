package com.rethinkscala.net

import org.jboss.netty.channel.socket.nio.NioClientSocketChannelFactory
import java.util.concurrent.Executors
import org.jboss.netty.bootstrap.ClientBootstrap
import java.net.InetSocketAddress

import org.jboss.netty.channel._

import ql2.{Response, VersionDummy}

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
import com.rethinkscala.ast.{After, WithLifecycle, Insert, Datum}
import org.jboss.netty.handler.codec.frame.FrameDecoder
import java.util.concurrent.atomic.AtomicInteger
import com.rethinkscala.utils.Helpers._
import scala.Some
import Translate._
import com.rethinkscala.Term
import scala.concurrent.duration.Duration


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

case class QueryToken[R](connection: Connection, query: ql2.Query, term: Term, p: Promise[R], mf: Manifest[R]) extends Token {


  implicit val t = mf

  private[rethinkscala] def context = connection

  type ResultType = R
  type MapType = Map[String, _]
  type IterableType = Iterable[MapType]

  def cast(value: Any, json: String): ResultType = value match {
    case v: MapType => translate[MapType, ResultType].read(v.asInstanceOf[MapType], json, term)
    case a: IterableType => translate[IterableType, ResultType].read(a.asInstanceOf[IterableType], json, term)
    case x: Any => x.asInstanceOf[R]

  }


  def toResult(response: Response) = {
    val (data: Any, json: String) = Datum.wrap(response.`response`(0))
    val rtn = data match {
      case None => None
      case _ => cast(data, json)
    }


    rtn


  }

  def handle(response: Response) = {
    (response.`type` match {

      case r@Some(ResponseType.RUNTIME_ERROR | ResponseType.COMPILE_ERROR | ResponseType.CLIENT_ERROR) => toError(response, term)
      case s@Some(ResponseType.SUCCESS_PARTIAL | ResponseType.SUCCESS_SEQUENCE) => toCursor(0, response)
      case Some(ResponseType.SUCCESS_ATOM) => toResult(response)
      case _ =>

    }) match {
      case e: Exception => p failure (e)
      case e: Any => p success (e.asInstanceOf[R])
    }
  }


  def toCursor(id: Int, response: Response) = {


    //val seqManifest = implicitly[Manifest[Seq[R]]]

    val seq = for (d <- response.`response`) yield (Datum.wrap(d) match {
      case (a: Any, json: String) => cast(a, json)
    })

    new Cursor[R](id, this, seq, response.`type` match {
      case Some(ResponseType.SUCCESS_SEQUENCE) => true
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
      case v: VersionDummy.Version.EnumVal => {
        val b = buffer(ByteOrder.LITTLE_ENDIAN, 4)
        b.writeInt(v.getNumber)
        b
      }
      case s: String => {
        val b = buffer(ByteOrder.LITTLE_ENDIAN, s.length + 4)
        b.writeInt(s.length)
        b.writeBytes(s.getBytes("UTF-8"))
        b
      }
      case q: ql2.Query => {
        val size = q.getSerializedSize
        val b = buffer(ByteOrder.LITTLE_ENDIAN, size + 4)
        b.writeInt(size)

        b.writeBytes(q.toByteArray)
        println(b)
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
    p.addLast("protobufDecoder", new ProtobufDecoder(Response.getDefaultInstance))

    //p.addLast("frameEncoder", new ProtobufVarint32LengthFieldPrepender());

    p.addLast("protobufEncoder", new RethinkDBEncoder())

    p.addLast("handler", defaultHandler)
    p
  }
}

object Connection {

  lazy val defaultConnection = Connection


}

case class Connection(version: Version, timeoutDuration: Duration = Duration(30, "seconds")) {

  private val defaultDB = Some(version.db.getOrElse("test"))
  private[this] val connectionId = new AtomicInteger();
  lazy val bootstrap = {

    val factory =
      new NioClientSocketChannelFactory(
        Executors.newCachedThreadPool(),
        Executors.newCachedThreadPool())

    val b = new ClientBootstrap(factory)
    b.setPipelineFactory(new PipelineFactory())
    b.setOption("tcpNoDelay", true)
    b.setOption("keepAlive", true)
    b.setOption("bufferFactory", new HeapChannelBufferFactory(ByteOrder.LITTLE_ENDIAN));
    b

  }

  case class ChannelWrapper(private val cf: ChannelFuture) {
    val id = connectionId.incrementAndGet()
    val token: AtomicInteger = new AtomicInteger();
    lazy val channel = cf.getChannel
  }


  protected val channel = new SimpleConnectionPool(new ConnectionFactory[ChannelWrapper] {

    def create(): ChannelWrapper = {

      val c = bootstrap.connect(new InetSocketAddress(version.host, version.port))
      c.addListener(new ChannelFutureListener {
        def operationComplete(future: ChannelFuture) {
          version.configure(future.getChannel)
        }
      })


      new ChannelWrapper(c)
    }

    def validate(wrapper: ChannelWrapper): Boolean = {
      wrapper.channel.isOpen
    }

    def destroy(wrapper: ChannelWrapper) {
      wrapper.channel.close()
    }
  }, max = version.maxConnections)

  import scala.concurrent.ExecutionContext.Implicits._

  protected[rethinkscala] def write[T](term: Term)(implicit mf: Manifest[T]): Promise[T] = {
    val p = promise[T]
    val f = p.future
    channel take {
      case (c, restore) => {
        val query = toQuery(term, c.token.getAndIncrement, defaultDB)


        val token = QueryToken[T](this, query, term, p, mf)
        // TODO : Check into dropping netty and using sockets for each,
        // Or find a way so that we can store the token for the netty handler to complete

        c.channel.setAttachment(token)
        c.channel.write(query)
        f onComplete {
          case _ => restore(c)
        }
      }
    }

    p
  }
}
