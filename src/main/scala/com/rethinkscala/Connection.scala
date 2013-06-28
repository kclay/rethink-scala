package com.rethinkscala

import org.jboss.netty.channel.socket.nio.NioClientSocketChannelFactory
import java.util.concurrent.Executors
import org.jboss.netty.bootstrap.ClientBootstrap
import java.net.InetSocketAddress

import org.jboss.netty.channel._
import org.jboss.netty.handler.codec.protobuf.ProtobufDecoder
import ql2.{ Response, VersionDummy }

import org.jboss.netty.handler.codec.oneone.OneToOneEncoder
import org.jboss.netty.buffer.ChannelBuffers._
import org.jboss.netty.buffer.{ ChannelBuffer, HeapChannelBufferFactory }
import java.nio.ByteOrder
import com.rethinkscala.utils.{ ConnectionFactory, SimpleConnectionPool }
import concurrent._
import org.jboss.netty.channel.Channels.pipeline
import com.rethinkscala.ConvertFrom._

import org.jboss.netty.channel.Channel
import ql2.Response.ResponseType
import com.rethinkscala.ast.Datum
import org.jboss.netty.handler.codec.frame.FrameDecoder
import java.util.concurrent.atomic.AtomicInteger
import com.rethinkscala.utils.Helpers._
import scala.Some
import scala.reflect.runtime.universe.TypeTag
import scala.reflect.ClassTag

/** Created by IntelliJ IDEA.
 *  User: Keyston
 *  Date: 3/23/13
 *  Time: 2:53 PM
 */

abstract class Token {
  type ResultType
  val query: ql2.Query
  val term: Term

  def success(value: Any, json: String)

  def failure(t: Throwable)
}

case class QueryToken[R](query: ql2.Query, term: Term, p: Promise[R], tt: Manifest[R]) extends Token {

  implicit val t = tt

  import Translate.translate

  val DocClass = classOf[Document]
  type MapType = Map[String, _]
  type IterableType = Iterable[MapType]

  def cast(value: Any, json: String): R = {

    value match {
      case v: MapType      => translate[MapType, R].read(v.asInstanceOf[MapType], json, term)
      case a: IterableType => translate[IterableType, R].read(a.asInstanceOf[IterableType], json, term)

    }

  }

  def success(value: Any, json: String) = p success (cast(value, json))

  def failure(t: Throwable) = p failure (t)

}

case class Cursor[T](channel: Channel, query: ql2.Query, term: Term, var chunks: Seq[T], completed: Boolean) {

}

class RethinkDBHandler extends SimpleChannelUpstreamHandler {

  implicit def channelHandlerContext2Promise(ctx: ChannelHandlerContext): Option[Token] = Some(ctx.getChannel.getAttachment.asInstanceOf[Token])

  override def messageReceived(ctx: ChannelHandlerContext, e: MessageEvent) {

    ctx.map {
      token =>
        {
          val response = e.getMessage.asInstanceOf[Response]

          //for(d <- response.getResponseList) yield d

          (response.`type` match {

            case r @ Some(ResponseType.RUNTIME_ERROR | ResponseType.COMPILE_ERROR | ResponseType.CLIENT_ERROR) => toError(response, token term)
            case s @ Some(ResponseType.SUCCESS_PARTIAL | ResponseType.SUCCESS_SEQUENCE) => Cursor(ctx.getChannel, token query, token term, Seq.empty[AnyRef], s == ResponseType.SUCCESS_SEQUENCE)
            case Some(ResponseType.SUCCESS_ATOM) => if (response.`response`.nonEmpty) Some(Datum.wrap(response.`response`(0))) else None
            case _ =>

          }) match {
            case e: Exception                 => token failure (e)

            case Some((a: Any, json: String)) => token success (a, json)

          }

          //  token success (e.getMessage)

        }

    }

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

case class Connection(version: Version) {

  private val defaultDB = Some(version.db.getOrElse("test"))
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

  case class ChannelWrapper(channel: Channel) {
    val token: AtomicInteger = new AtomicInteger();
  }

  protected val channel = new SimpleConnectionPool(new ConnectionFactory[ChannelWrapper] {

    def create(): ChannelWrapper = {

      val c = bootstrap.connect(new InetSocketAddress(version.host, version.port)).await().getChannel
      version.configure(c)
      new ChannelWrapper(c)
    }

    def validate(wrapper: ChannelWrapper): Boolean = {
      wrapper.channel.isOpen
    }

    def destroy(wrapper: ChannelWrapper) {
      wrapper.channel.close()
    }
  }, max = version.maxConnections)

  def write[T](term: Term)(implicit tt: Manifest[T]): Future[T] =
    channel[Future[T]]() {
      c =>

        val query = toQuery(term, c.token.getAndIncrement, defaultDB)
        val p = promise[T]()
        val token = QueryToken[T](query, term, p, tt)
        c.channel.setAttachment(token)
        c.channel.write(query)
        p.future

    }

}
