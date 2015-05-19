package com.rethinkscala

import com.rethinkscala.net.DefaultCursor
import org.scalatest.concurrent._
import org.scalatest.{FunSuite, Matchers}
import Async._

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 5/31/14
 * Time: 1:42 PM
 *
 */
class AsyncTest extends FunSuite with WithBase with ScalaFutures with Matchers {


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
}
