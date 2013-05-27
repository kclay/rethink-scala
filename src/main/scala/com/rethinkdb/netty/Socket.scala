package com.rethinkdb.netty

import org.jboss.netty.channel.socket.nio.NioClientSocketChannelFactory
import java.util.concurrent.Executors
import org.jboss.netty.bootstrap.ClientBootstrap
import java.net.InetSocketAddress

import org.jboss.netty.channel._
import org.jboss.netty.handler.codec.protobuf.{ProtobufEncoder, ProtobufVarint32FrameDecoder, ProtobufVarint32LengthFieldPrepender, ProtobufDecoder}
import ql2.{Query, Response, VersionDummy}

import org.jboss.netty.handler.codec.oneone.OneToOneEncoder
import com.google.protobuf.{CodedInputStream, MessageLite}
import org.jboss.netty.buffer.ChannelBuffers._
import org.jboss.netty.buffer.{ChannelBuffer, HeapChannelBufferFactory}
import java.nio.ByteOrder
import com.rethinkdb.utils.{ConnectionFactory, SimpleConnectionPool}
import concurrent._
import org.jboss.netty.channel.Channels.pipeline
import com.rethinkdb.{RethinkError, Term}
import com.rethinkdb.ConvertFrom._


import org.jboss.netty.channel.Channel
import ql2.Response.ResponseType
import com.rethinkdb.ast.Datum
import org.jboss.netty.handler.codec.frame.{CorruptedFrameException, FrameDecoder}
import scala.collection.mutable.ArrayBuffer


/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 3/23/13
 * Time: 2:53 PM 
 */


case class QueryToken(query: Query, term: Term, promise: Promise[Any]) {

  def success(value: Any) = promise success (value)

  def failure(t: Throwable) = promise failure (t)
}


case class Cursor[T](channel: Channel, query: Query, term: Term, var chunks: Seq[T], completed: Boolean) {

}

class RethinkDBHandler extends SimpleChannelUpstreamHandler {


  implicit def channelHandlerContext2Promise(ctx: ChannelHandlerContext): Option[QueryToken] = Some(ctx.getChannel.getAttachment.asInstanceOf[QueryToken])

  override def messageReceived(ctx: ChannelHandlerContext, e: MessageEvent) {

    ctx.map {
      token => {
        val response = e.getMessage.asInstanceOf[Response]

        //for(d <- response.getResponseList) yield d



        (response.`type` match {

          case r@Some(ResponseType.RUNTIME_ERROR | ResponseType.COMPILE_ERROR | ResponseType.CLIENT_ERROR) => toError(response, token term)
          case s@Some(ResponseType.SUCCESS_PARTIAL | ResponseType.SUCCESS_SEQUENCE) => Cursor(ctx.getChannel, token query, token term, Seq.empty[AnyRef], s == ResponseType.SUCCESS_SEQUENCE)
          case Some(ResponseType.SUCCESS_ATOM) => if (response.`response`.nonEmpty) Some(Datum.unapply(response.`response`(0))) else None
          case _=>


        }) match {
          case e: Exception => token failure (e)
          case a: Any => token success (a)
        }



        token success (e.getMessage)

      }

    }

  }

  override def exceptionCaught(ctx: ChannelHandlerContext, e: ExceptionEvent) {

    ctx.map(_.failure(e.getCause))
  }
}

private class RethinkDBFrameDecoder extends FrameDecoder {
  def decode(ctx: ChannelHandlerContext, channel: Channel, buffer: ChannelBuffer): AnyRef = {
    buffer.markReaderIndex

    if(!buffer.readable() || buffer.readableBytes()<4){
      buffer.resetReaderIndex()
      return null
    }

    val length= buffer.readInt()

    if(buffer.readableBytes()<length){
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
      case q: Query => {
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


trait Socket[T] {

  import ExecutionContext.Implicits.global

  val host: String
  val port: Int
  val maxConnections: Int
  lazy val bootstrap = {

    val factory =
      new NioClientSocketChannelFactory(
        Executors.newCachedThreadPool(),
        Executors.newCachedThreadPool())

    val b = new ClientBootstrap(factory)
    b.setPipelineFactory(new PipelineFactory())
    b.setOption("tcpNoDelay", true)
    b.setOption("keepAlive", true)
    b.setOption("bufferFactory", new
        HeapChannelBufferFactory(ByteOrder.LITTLE_ENDIAN));
    b


  }
  protected val channel = new SimpleConnectionPool(new ConnectionFactory[Channel] {

    def create(): Channel = {

      val c = bootstrap.connect(new InetSocketAddress(host, port)).await().getChannel
      c.write(VersionDummy.Version.V0_1).await()
      c
    }


    def validate(connection: Channel): Boolean = {
      connection.isOpen
    }

    def destroy(connection: Channel) {
      connection.close()
    }
  }, max = maxConnections)

  def write(query: Query, term: Term): T

  protected def _write(query: Query, term: Term): Future[Any] = {
    val p = promise[Any]
    val f = p.future
    // add this to a future
    future {
      channel() {
        c =>

          c.setAttachment(QueryToken(query, term, p))
          c.write(query).await()

      }

    }
    f
  }


}

case class BlockingSocket(host: String, port: Int, maxConnections: Int = 5) extends Socket[Any] {
  def write(query: Query, term: Term):Any = {
    blocking(_write(query, term))
  }
}

case class AsyncSocket(host: String, port: Int, maxConnections: Int = 5) extends Socket[Future[Any]] {
  def write(query: Query, term: Term):Future[Any] = {
    _write(query, term)
  }
}

