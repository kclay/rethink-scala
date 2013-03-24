package com.rethinkdb.netty

import org.jboss.netty.channel.socket.nio.NioClientSocketChannelFactory
import java.util.concurrent.Executors
import org.jboss.netty.bootstrap.ClientBootstrap
import java.net.InetSocketAddress

import org.jboss.netty.channel._
import org.jboss.netty.handler.codec.protobuf.{ProtobufEncoder, ProtobufVarint32LengthFieldPrepender, ProtobufDecoder, ProtobufVarint32FrameDecoder}
import ql2.Ql2.{Term, Datum, Query, Response}
import org.jboss.netty.handler.codec.oneone.OneToOneEncoder
import java.lang.Object
import com.google.protobuf.MessageLite
import org.jboss.netty.buffer.ChannelBuffers._
import org.jboss.netty.channel.ChannelHandler.Sharable
import org.jboss.netty.buffer.{ChannelBuffers, HeapChannelBufferFactory}
import java.nio.ByteOrder


/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 3/23/13
 * Time: 2:53 PM 
 */

import org.jboss.netty.channel.Channels.pipeline
import ql2.Ql2.VersionDummy
import concurrent.{Promise, promise}

case class Cursor(channel:Channel,query:Query,term:Term,completed:Boolean){

}
class RethinkDBHandler extends SimpleChannelUpstreamHandler {

  implicit def channelHandlerContext2Promise(ctx:ChannelHandlerContext):Promise[AnyRef]=ctx.getAttachment.asInstanceOf[Promise[AnyRef]]
  override def messageReceived(ctx: ChannelHandlerContext, e: MessageEvent) {

      ctx success(e.getMessage)

  }

  override def exceptionCaught(ctx: ChannelHandlerContext, e: ExceptionEvent) {
    ctx failure(e.getCause)
  }
}

private class RethinkDBEncoder extends OneToOneEncoder {


  def encode(ctx: ChannelHandlerContext, channel: Channel, msg: Any): AnyRef = {

    msg match{
      case v:VersionDummy.Version=> {
        val b=buffer(ByteOrder.LITTLE_ENDIAN,8)
        b.writeLong(v.getNumber)
        b
      }
      case q:Query =>{
        val size  = q.getSerializedSize
       val b= buffer(ByteOrder.LITTLE_ENDIAN,size+8)
       b.writeLong(size)
       b.writeBytes(q.toByteArray)
        b
      }
    }

  }
}

private class PipelineFactory extends ChannelPipelineFactory {
  val defaultHandler =  new RethinkDBHandler()
  def getPipeline: ChannelPipeline = {
    val newPipeline = pipeline()

    newPipeline.addLast("protobufDecoder", new ProtobufDecoder(Response.getDefaultInstance))


    newPipeline.addLast("protobufEncoder", new RethinkDBEncoder())

    newPipeline.addLast("handler",defaultHandler)
    newPipeline
  }
}

case class Socket(host: String, port: Int) {

  lazy val channel = {
    val factory =
      new NioClientSocketChannelFactory(
        Executors.newCachedThreadPool(),
        Executors.newCachedThreadPool())

    val bootstrap = new ClientBootstrap(factory)
    bootstrap.setPipelineFactory(new PipelineFactory())
    bootstrap.setOption("tcpNoDelay", true)
    bootstrap.setOption("keepAlive", true)
    bootstrap.setOption("bufferFactory", new
        HeapChannelBufferFactory(ByteOrder.LITTLE_ENDIAN));
    val c =bootstrap.connect(new InetSocketAddress(host, port)).await().getChannel
    c.write(VersionDummy.Version.V0_1) .await()
    c

  }

  def write(query:Query) = {
     val channelFuture = channel.write(query)
   // RethinkDBHandler.
    val p=promise[AnyRef]
    val f=p.future

    channelFuture.getChannel.setAttachment(p)
    f
  }
}


