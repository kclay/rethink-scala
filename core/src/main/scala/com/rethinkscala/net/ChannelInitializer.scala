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


trait RethinkChannelInitializer extends ChannelInitializer[SocketChannel]

object JsonChannelInitializer {
  def apply(version: Version3) = new RethinkChannelInitializer {

    override def initChannel(ch: SocketChannel) = {
      val pipe = ch.pipeline()
      val promise = ch.newPromise()
      ChannelAttribute.Future.set(ch, promise)

      pipe.addLast(new RethinkConfigureHandler(version))
      pipe.addLast("encoder", new RethinkDBEncoder)
    }
  }
}


object ProtoChannelInitializer extends RethinkChannelInitializer {


  //val protoDecoder = ProtobufDecoder(Response.getDefaultInstance)


  override def initChannel(ch: SocketChannel) = {
    // super.initChannel(ch)
    // ch.pipeline().addAfter("frameDecoder", "protoDecoder", protoDecoder)
  }


}

