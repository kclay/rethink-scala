package com.rethinkscala.backend.netty

import com.rethinkscala.net.AbstractJsonVersion
import io.netty.channel.ChannelInitializer
import io.netty.channel.socket.SocketChannel

/**
  * Created with IntelliJ IDEA.
  * User: keyston
  * Date: 8/16/14
  * Time: 11:52 AM
  *
  */


trait RethinkChannelInitializer extends ChannelInitializer[SocketChannel]

object JsonChannelInitializer {
  def apply(version: AbstractJsonVersion): RethinkChannelInitializer = new RethinkChannelInitializer {

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

