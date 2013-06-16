package com.rethinkdb

import org.jboss.netty.channel.Channel

import ql2.VersionDummy
import com.rethinkdb.ast.DB

abstract class Version {

  val host: String
  val port: Int
  val maxConnections: Int

  def configure(c: Channel)
}
case class Version1(host: String = "localhost", port: Int = 28015, db: Option[String] = None, maxConnections: Int = 5) extends Version {
  def configure(c: Channel) {
    c.write(VersionDummy.Version.V0_1).await()
  }

}

case class Version2(host: String = "localhost", port: Int = 28015, maxConnections: Int = 5, authKey: String = "") extends Version {
  def configure(c: Channel) {
    c.write(VersionDummy.Version.V0_1)
    c.write(authKey).await()
  }
}
