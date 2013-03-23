package com.rethinkdb.netty

import org.jboss.netty.channel.socket.nio.NioClientSocketChannelFactory
import java.util.concurrent.Executors
import org.jboss.netty.bootstrap.ClientBootstrap
import java.net.InetSocketAddress

import org.jboss.netty.channel._
import org.jboss.netty.handler.codec.protobuf.{ProtobufEncoder, ProtobufVarint32LengthFieldPrepender, ProtobufDecoder, ProtobufVarint32FrameDecoder}
import ql2.Ql2.{Term, Datum, Query, Response}




/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 3/23/13
 * Time: 2:53 PM 
 */

import org.jboss.netty.channel.Channels.pipeline
import concurrent.{Promise, promise}

case class Cursor(channel:Channel,query:Query,term:Term,completed:Boolean){

}
class RethinkDBHandler extends SimpleChannelUpstreamHandler {

  implicit def channelHandlerContext2Promise(ctx:ChannelHandlerContext):Promise[AnyRef]=ctx.getAttachment.asInstanceOf[Promise[AnyRef]]
  override def messageReceived(ctx: ChannelHandlerContext, e: MessageEvent) {


  }
}
private class PipelineFactory extends ChannelPipelineFactory {
  val defaultHandler =  new RethinkDBHandler()
  def getPipeline: ChannelPipeline = {
    val newPipeline = pipeline()
    newPipeline.addLast("frameDecoder", new ProtobufVarint32FrameDecoder())
    newPipeline.addLast("protobufDecoder", new ProtobufDecoder(Response.getDefaultInstance))

    newPipeline.addLast("frameEncoder", new ProtobufVarint32LengthFieldPrepender())
    newPipeline.addLast("protobufEncoder", new ProtobufEncoder())

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
    bootstrap.connect(new InetSocketAddress(host, port)).await().getChannel

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


