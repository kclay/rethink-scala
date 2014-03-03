package com.rethinkscala

import org.scalatest.FunSuite
import org.scalatest.concurrent.Futures

import com.rethinkscala.net._
import java.util.concurrent.{TimeUnit, LinkedBlockingQueue}
import com.rethinkscala.net.BlockingConnection
import com.rethinkscala.net.Version2
import com.rethinkscala.ast.Expr
import Blocking._


/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 12/16/13
 * Time: 11:46 AM 
 */
class ConnectionTest extends FunSuite with WithBase with Futures {

  import scala.concurrent.ExecutionContext.Implicits._

  override def setupDB = false

  def blockingConnection(authKey: String) = BlockingConnection(new Version2(host, port, authKey = authKey))

  def asyncConnection(authKey: String) = AsyncConnection(new Version2(host, port, authKey = authKey))

  test("v2 auth success") {


    val queue = new LinkedBlockingQueue[Boolean]
    val conn = blockingConnection("foobar")
    conn.channel take {
      case (c, restore) =>
        restore(c)
        queue.put(true)


    }


    assert(queue.poll(10, TimeUnit.SECONDS))
  }

  test("blocking query") {

    implicit val connection = blockingConnection("foobar")
    val e = Expr(1)

    val results = e run

    assert[Double](results, {
      t: Double => t == 1
    })


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
