package com.rethinkscala.ast

import org.scalatest.FunSuite
import com.rethinkscala.net.{JoinResult, Document}
import com.rethinkscala.{r, WithBase}

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 8/17/13
 * Time: 2:12 PM
 *
 */

case class FooJ(id:Int,value:Int) extends Document
case class BarJ(id:Int,value:Int) extends Document

class JoinTest extends FunSuite with WithBase{




  test("eq join should have left as Foo and right as Bar"){

    val foos = Table[FooJ]("foo",db=Some(db))
    val bars = Table[BarJ]("bar",db=Some(db))

    foos.create.run
    bars.create.run

    foos.insert(FooJ(1,value=1)).run
    bars.insert(BarJ(1,value=1)).run

   // println(foos.run)

    val join = foos.eqJoin("value",bars)

    val results = join.toOpt
    assert(results.isDefined)

    assert(results.get.size == 1)
    val head = results.get.head
    assert(head.left == FooJ(1,1) && head.right==BarJ(1,1))



  }


}
