package com.rethinkscala.utils

import java.util.concurrent.atomic.AtomicBoolean

import scala.concurrent.{ExecutionContext, Future}

/** Created by IntelliJ IDEA.
  * User: Keyston
  * Date: 3/24/13
  * Time: 8:33 PM
  *
  */

//https://github.com/jamesgolick/scala-connection-pool/
trait ConnectionFactory[Connection] {
  def create(): Connection

  def validate(connection: Connection): Boolean

  def configure(connection: Connection): Unit

  def destroy(connection: Connection): Unit
}

trait AbstractConnectionPool[Connection] {

  def getConnectionById(id: Long): Option[Connection]

  def take(connectionId: Option[Long], name: Option[String] = None)(block: (Connection, Connection => Unit, Connection => Unit) => Unit)(implicit exc: ExecutionContext): Future[Connection]

}


trait ConnectionWithId {


  val id: Long
  var active: AtomicBoolean
}

trait LowLevelConnectionPool[Connection] {
  def borrow(): Connection

  def giveBack(conn: Connection): Unit

  def invalidate(conn: Connection): Unit
}

class TimeoutError(message: String) extends Error(message)

class SimpleConnectionPool[Conn <: ConnectionWithId](connectionFactory: ConnectionFactory[Conn],
                                                     max: Int = 20,
                                                     timeout: Int = 5000)
