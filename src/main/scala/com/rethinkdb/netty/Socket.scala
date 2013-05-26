package com.rethinkdb.netty

import org.jboss.netty.channel.socket.nio.NioClientSocketChannelFactory
import java.util.concurrent.Executors
import org.jboss.netty.bootstrap.ClientBootstrap
import java.net.InetSocketAddress

import org.jboss.netty.channel._
import org.jboss.netty.handler.codec.protobuf.ProtobufDecoder
import ql2.{Query, Response, VersionDummy}

import org.jboss.netty.handler.codec.oneone.OneToOneEncoder
import com.google.protobuf.MessageLite
import org.jboss.netty.buffer.ChannelBuffers._
import org.jboss.netty.buffer.{ChannelBuffer, HeapChannelBufferFactory}
import java.nio.ByteOrder
import com.rethinkdb.utils.{ConnectionFactory, SimpleConnectionPool}
import concurrent._
import org.jboss.netty.channel.Channels.pipeline
import com.rethinkdb.RTerm


import org.jboss.netty.channel.Channel


/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 3/23/13
 * Time: 2:53 PM 
 */


case class QueryToken(query: Query, term: RTerm, promise: Promise[AnyRef]) {

  def success(value: AnyRef) = promise success (value)

  def failure(t: Throwable) = promise failure (t)
}


case class Cursor[T](channel: Channel, query: Query, term: RTerm, var chunks: Seq[T], completed: Boolean) {

}

class RethinkDBHandler extends SimpleChannelUpstreamHandler {


  implicit def channelHandlerContext2Promise(ctx: ChannelHandlerContext): Option[QueryToken] = Option(ctx.getAttachment.asInstanceOf[QueryToken])

  override def messageReceived(ctx: ChannelHandlerContext, e: MessageEvent) {
    ctx.map {
      token => {
        val response = e.getMessage.asInstanceOf[Response]

        //for(d <- response.getResponseList) yield d

        /*response.`type`.map()
        response.match{
          case  r @(RUNTIME_ERROR |COMPILE_ERROR | CLIENT_ERROR)=>toError(response, token term)
          case  s@(SUCCESS_PARTIAL| SUCCESS_SEQUENCE)=>Cursor(ctx.getChannel,token query,token term,Seq.empty[AnyRef],s == SUCCESS_SEQUENCE)


        } */

        token success (e.getMessage)

      }

    }

  }

  override def exceptionCaught(ctx: ChannelHandlerContext, e: ExceptionEvent) {

    ctx.map(_.failure(e.getCause))
  }
}

private case class RethinkDBDecoder(prototype: MessageLite) extends ProtobufDecoder(prototype) {
  override def decode(ctx: ChannelHandlerContext, channel: Channel, msg: Any): AnyRef = {
    val buf: ChannelBuffer = msg.asInstanceOf[ChannelBuffer]
    val headerLength = buf.readInt()
    super.decode(ctx, channel, msg)
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
    val newPipeline = pipeline()

    newPipeline.addLast("protobufDecoder", new RethinkDBDecoder(Response.getDefaultInstance))


    newPipeline.addLast("protobufEncoder", new RethinkDBEncoder())

    newPipeline.addLast("handler", defaultHandler)
    newPipeline
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

  def write(query: Query, term: RTerm): T

  protected def _write(query: Query, term: RTerm): Future[AnyRef] = {
    val p = promise[AnyRef]
    val f = p.future
    // add this to a future
    future {
      channel() {
        c =>
          c.write(query)
          val channelFuture = c.write(query)
          // RethinkDBHandler.


          channelFuture.getChannel.setAttachment(QueryToken(query, term, p))
      }

    }
    f
  }


}

case class BlockingSocket(host: String, port: Int, maxConnections: Int = 5) extends Socket[AnyRef] {
  def write(query: Query, term: RTerm): AnyRef = {
    blocking(_write(query, term))
  }
}

case class AsyncSocket(host: String, port: Int, maxConnections: Int = 5) extends Socket[Future[AnyRef]] {
  def write(query: Query, term: RTerm) = {
    _write(query, term)
  }
}

