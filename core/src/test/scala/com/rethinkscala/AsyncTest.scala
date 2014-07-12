package com.rethinkscala

import org.scalatest.concurrent._
import org.scalatest.{FunSuite, Matchers}


/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 5/31/14
 * Time: 1:42 PM
 *
 */
class AsyncTest extends FunSuite with WithBase with ScalaFutures with Matchers{



  test("async"){
    val  res = r.expr(1) === 1


    val f = async(res)


    whenReady(async(res)) { b=>

      assert(b)
    }








  }
}
