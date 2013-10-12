package com.rethinkscala.ast

import org.scalatest.FunSuite
import com.rethinkscala.{r, WithBase}
import org.joda.time.DateTime

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 10/4/13
 * Time: 10:56 AM
 *
 */
class TimeTest  extends FunSuite with WithBase{


  test("r.now"){
    val now = new DateTime()
    assert(r.now,{
      dt:DateTime =>{
        println(dt)
        println(now)
        dt.dayOfMonth().get() ==now.dayOfMonth().get() && dt.dayOfWeek().get() == dt.dayOfWeek().get()

      }
    })
  }
}
