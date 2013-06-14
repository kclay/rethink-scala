package com.rethinkdb

import org.jboss.netty.channel.Channel
import com.rethinkdb.netty.Socket


sealed trait Version{

  def init(c:Channel,socket:Socket)
}
case object Version1 extends Version{
  def init(c: Channel,socket:Socket) {

  }
}

case object Version2 extends Version{
  def init(c: Channel,socket:Socket) {

  }
}
