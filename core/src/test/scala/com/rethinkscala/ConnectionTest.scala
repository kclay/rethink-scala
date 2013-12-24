package com.rethinkscala

import org.scalatest.FunSuite
import com.rethinkscala.net.{Connection, Version2}
import scala.actors.threadpool.LinkedBlockingQueue
import scala.actors.threadpool.TimeUnit

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 12/16/13
 * Time: 11:46 AM 
 */
class ConnectionTest extends FunSuite with WithBase {

  import scala.concurrent.ExecutionContext.Implicits._

  override def setupDB = false

  def newConnection(authKey: String) = {


    new Connection(new Version2(host, port, authKey = authKey))
  }

  test("v2 auth success") {


    val queue = new LinkedBlockingQueue[Boolean]
    val conn = newConnection("foobar")
    conn.channel take {
      case (c, restore) =>
        restore(c)
        queue.put(true)


    }


    assert(queue.poll(10, TimeUnit.SECONDS))
  }
  /*
  test("v2 auth failed") {

    val conn = newConnection("foobar2")
    val queue = new LinkedBlockingQueue[Boolean]


    conn.channel take {
      case (c, restore) =>
        restore(c)


    } onFailure {
      case e: RethinkDriverError => queue.put(true)
    }


    assert(queue.poll(10, TimeUnit.SECONDS))
  } */


}
