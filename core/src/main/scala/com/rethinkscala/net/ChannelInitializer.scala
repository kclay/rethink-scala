package com.rethinkscala.net

import io.netty.channel.socket.SocketChannel
import io.netty.channel.{ChannelHandlerAdapter, ChannelInitializer}

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 8/16/14
 * Time: 11:52 AM
 *
 */


trait RethinkChannelInitializer extends ChannelInitializer[SocketChannel] {


  val defaultHandler: ChannelHandlerAdapter

  def frameDecoder: RethinkFrameDecoder

  val encoder = new RethinkDBEncoder

  override def initChannel(ch: SocketChannel) = {
    val pipe = ch.pipeline()
    pipe.addLast("frameDecoder", frameDecoder)
    pipe.addLast("encoder", encoder)
    pipe.addLast(defaultHandler)
  }
}

object JsonChannelInitializer extends RethinkChannelInitializer {

  override val defaultHandler = new JsonChannelHandler

  override def frameDecoder: RethinkFrameDecoder = new JsonFrameDecoder()
}

object ProtoChannelInitializer extends RethinkChannelInitializer {

  override val defaultHandler = new ProtoChannelHandler

  override def frameDecoder: RethinkFrameDecoder = new ProtoFrameDecoder()

  //val protoDecoder = ProtobufDecoder(Response.getDefaultInstance)


  override def initChannel(ch: SocketChannel) = {
    super.initChannel(ch)
   // ch.pipeline().addAfter("frameDecoder", "protoDecoder", protoDecoder)
  }


}

