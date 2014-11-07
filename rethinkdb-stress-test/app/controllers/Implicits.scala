package controllers

import com.rethinkscala.net.{BlockingConnection, Version3}

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 11/6/2014
 * Time: 7:32 AM 
 */
object Implicits {
  val host = "127.0.0.1"
  val port = 28015
  val authKey = "foobar"
  implicit val connection = BlockingConnection(new Version3(host, port, authKey = authKey))

}
