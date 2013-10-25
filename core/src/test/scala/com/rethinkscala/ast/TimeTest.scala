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

        dt.dayOfMonth().get() ==now.dayOfMonth().get() && dt.dayOfWeek().get() == dt.dayOfWeek().get()

      }
    })
  }

  test("time.dayOfWeek"){

    val now = new DateTime()


    val dayOfWeek = r.weekdays(now.dayOfWeek().get()-1)

    assert(r.now.dayOfWeek.eq(dayOfWeek))
  }

  test("time.day"){

    val now = new DateTime()

    assert(r.now.day.eq(now.getDayOfMonth))
  }
}
