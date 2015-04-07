package com.rethinkscala.net

import io.netty.channel.ChannelInitializer
import io.netty.channel.socket.SocketChannel

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 4/7/2015
 * Time: 10:58 AM 
 */
class RethinkClientInitializer extends ChannelInitializer[SocketChannel]{
  override def initChannel(ch: SocketChannel) = {
    val pipe = ch.pipeline();
  }
}
