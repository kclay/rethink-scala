package com.rethinkdb

import org.jboss.netty.channel.Channel

import ql2.VersionDummy


abstract class Version{

  val host:String
  val port:Int
  val maxConnections:Int

  def configure(c:Channel)
}
case class Version1(host: String = "localhost", port: Int = 28015, maxConnections: Int = 5) extends Version{
  def configure(c: Channel) {
    c.write(VersionDummy.Version.V0_1).await()
  }



}

case class Version2(override val host: String = "localhost", override val port: Int = 28015, override val maxConnections: Int = 5,authKey:String="") extends Version1(host,port,maxConnections){
  override def configure(c: Channel) {
    super.configure(c)
    c.write(authKey).await()
  }
}
