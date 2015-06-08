package com.rethinkscala

import com.rethinkscala.net.DefaultCursor
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.concurrent._
import org.scalatest.{FunSuite, Matchers}
import Async._
import scala.concurrent.duration._


/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 5/31/14
 * Time: 1:42 PM
 *
 */
class AsyncTest extends FunSuite with WithBase with ScalaFutures with Matchers {

  import scala.concurrent.ExecutionContext.Implicits._

  test("async") {


    val cursor = async {
      implicit c =>


        r.expr(Seq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
          .withOptions(Map("max_batch_rows" -> 1)).run


    }




    whenReady(cursor) { cur =>

      println(cur)
    }


  }

  test("async continue") {


    val tbl = table.to[ConnectionPoolMsg]
    val entries = for (i <- 1 to 1000) yield ConnectionPoolMsg.create
    //block(tbl.insert(entries))

    val cursor = async {
      implicit c =>
       r.range(1000).withOptions(Map("max_batch_rows" -> 1)).run
    }

    whenReady(cursor, Timeout(30 seconds)) {
      results =>
        val values = results.map(i => i).toSet

        println(values.size == 1000)
    }
  }
}
