package com.rethinkscala

import org.scalatest.FunSuite
import com.rethinkscala.net.AsyncConnection


/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 5/31/14
 * Time: 1:42 PM
 *
 */
class AsyncTest extends FunSuite with WithBase{


  test("async"){
    val  res = r.expr(1) === 1

    val f = async(connection){
      implicit c: AsyncConnection =>
         c(res)

    }


    print(f)



  }
}
