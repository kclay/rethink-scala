package com.rethinkscala

import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}

import com.rethinkscala.Blocking._
import com.rethinkscala.ast.MakeArray
import com.rethinkscala.net._
import org.scalatest.FunSuite
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.concurrent.{ScalaFutures, Futures}


/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 12/16/13
 * Time: 11:46 AM 
 */

case class Hello(a: Boolean)

class ConnectionTest extends FunSuite with WithBase with ScalaFutures {

  import scala.concurrent.ExecutionContext.Implicits._

  override def setupDB = false

  def blockingConnection(authKey: String) = BlockingConnection(new Version3(host, port, authKey = authKey))

  def asyncConnection(authKey: String) = AsyncConnection(new Version3(host, port, authKey = authKey))

  test("v2 auth success") {


    val queue = new LinkedBlockingQueue[Boolean]
    val conn = blockingConnection("foobar")
    conn.pool.take(None) {
      case (c, restore, invalidate) =>
        restore(c)
        queue.put(true)


    }


    assert(queue.poll(10, TimeUnit.SECONDS))
  }

  test("v3 auth success") {

    val queue = new LinkedBlockingQueue[Boolean]
    val conn = BlockingConnection(new Version3(host, port, authKey = "foobar"))
    conn.pool.take(None) {
      case (c, restore, invalidate) =>
        restore(c)
        queue.put(true)


    }


    assert(queue.poll(10, TimeUnit.SECONDS))
  }

  test("blocking query") {

    implicit val connection = blockingConnection("foobar")
    val e = Expr(1)

    val results = e run

    assert(results.fold(x => false, x => x == 1))


  }

  test("restore connection on error") {


    val s: MakeArray[Any] = Expr(Seq(1, 2))
    assert(connection.pool.available == 0)

    val futureResult = async(s.asInstanceOf[MakeArray[Hello]])


    import scala.concurrent.duration._


    whenReady(futureResult.failed, Timeout(30 seconds)) { result =>
      assert(connection.pool.available == 0)
      result should be(asInstanceOf[RethinkRuntimeError])
    }


  }


  test("stress test") {

    implicit val connection = blockingConnection("foobar")
    val tbl = r.tableAs[ConnectionPoolMsg]("CheckInHitMsg")
    tbl.create.run
    val entries = for (i <- 0 to 10) yield ConnectionPoolMsg.create
    tbl.insert(entries).run


    var count = 0
    do {
      val res = tbl.orderBy(r.desc("timestamp")).toOpt

      if (res.isEmpty) {
        throw new UnknownError()
      }

      count = count + 1


    } while (count < 10)

  }
  /*
  test("v2 auth failed") {

    val conn = newConnection("foobar2")
    val queue = new LinkedBlockingQueue[Boolean]


    val f = conn.channel take {
      case (c, restore) =>
        restore(c)


    }


    f onFailure {
      case e: Exception =>
        println(e)
        queue.put(true)
    }




    val value = queue.poll(5, TimeUnit.SECONDS)
    println(f.value)
    assert(value)
  }     */


}
