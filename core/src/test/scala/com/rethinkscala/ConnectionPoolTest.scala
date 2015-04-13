package com.rethinkscala

import com.google.testing.threadtester.{SecondaryRunnableImpl, InterleavedRunner, MainRunnableImpl}
import com.rethinkscala.net.{ConnectionChannel, AbstractConnection}
import com.rethinkscala.test.utils.BlockingProxy
import com.rethinkscala.utils.{AbstractConnectionPool, RethinkConnectionPool}

import org.scalatest.FunSuite
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.concurrent.ScalaFutures

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Success

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 4/10/2015
 * Time: 7:46 AM 
 */

case class PoolMain(pool: AbstractConnectionPool[ConnectionChannel]) extends MainRunnableImpl[AbstractConnectionPool[ConnectionChannel]] {


  override def getClassUnderTest = classOf[AbstractConnectionPool[ConnectionChannel]]

  override def getMainObject = pool

  override def run() = {
    import scala.concurrent.ExecutionContext.Implicits.global
    System.out.printf("Running main thread %s\n", Thread.currentThread())
    pool.take(Some(1)) {
      case (connection, restore, invalidate) =>
        restore(connection)
        connection.active.set(true)

    }
    System.out.printf("Main thread finished\n")
  }
}

class PoolNotResolvedException extends Exception

class PoolSecond extends SecondaryRunnableImpl[AbstractConnectionPool[ConnectionChannel], PoolMain] {

  var pool: AbstractConnectionPool[ConnectionChannel] = _

  override def initialize(main: PoolMain) = {
    pool = main.getMainObject
  }

  override def run() = {
    import scala.concurrent.ExecutionContext.Implicits.global
    val f = pool.take(Some(1)) {
      case (connection, restore, invalidate) =>
    }
    f.value.orElse(throw new PoolNotResolvedException())

  }
}

class ConnectionPoolTest extends FunSuite with WithBase with ScalaFutures {


  type CP = AbstractConnectionPool[ConnectionChannel]
  test("checked out connection") {

    import scala.concurrent.ExecutionContext.Implicits.global
    val pool: CP = connection.pool

    var _restore: ConnectionChannel => Unit = null
    val duration = Timeout(10.seconds)
    val connectionId = Some(1L)
    var _connection: ConnectionChannel = null
    val connection1 = pool.take(connectionId) {
      case (conn, restore, invalidate) =>
        _restore = restore
        conn.active.set(true)

    }
    whenReady(connection1, duration) { result =>
      _connection = result
      assert(result != null)

    }


    var connection2Ok: Option[Boolean] = None
    val connection2 = pool.take(connectionId) {
      case (conn, restore, invalidate) =>
        connection2Ok = Some(true)
        restore(conn)
    }

    assert(!connection2.isReadyWithin(duration.value))




    assert(_connection.active.get())
    _restore(_connection)


    whenReady(connection2, duration) { result =>
      assert(connection2Ok.getOrElse(false))

    }
    assert(!_connection.active.get())

  }
}
