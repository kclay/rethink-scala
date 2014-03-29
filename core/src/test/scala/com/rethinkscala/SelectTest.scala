package com.rethinkscala

import org.scalatest.FunSuite
import com.rethinkscala.ast.Table


/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 7/2/13
 * Time: 11:46 AM
 *
 */


case class SelectFoo(id: Int) extends Document

class SelectTest extends FunSuite with WithBase {

  import connection.delegate._


  test("select between") {

    val records = for (i <- 1 to 50) yield SelectFoo(i)
    foos.insert(records).run








    assert(foos.between(10, 20).orderBy("id"), {
      f: Seq[SelectFoo] => f.size == 10 & f(0).id == 10 & f.last.id == 19
    })

    assert(foos.between(10, 20,BetweenOptions(leftBound = Some(Bound.Closed),rightBound = Some(Bound.Closed))).orderBy("id"), {
      f: Seq[SelectFoo] => f.size == 11 & f.last.id==20
    })

    assert(foos.between(10, 20,BetweenOptions(leftBound = Some(Bound.Open),rightBound = Some(Bound.Closed))).orderBy("id"), {
      f: Seq[SelectFoo] => f.size == 10 & f.last.id==20  && f(0).id == 11
    })




  }

  test("table select"){
    assert(foos,{
      f:Seq[SelectFoo]=> f.size == 50
    })
  }
  test("table select ordered") {

    val results = foos.orderBy("id")
    assert(results, {
      f: Seq[SelectFoo] => f.size == 50 && f.last.id == 50
    })
  }

  test("foos.get") {


    assert(foos.get(1), {
      f: SelectFoo => f.id == 1
    })
  }
  test("get_all") {
    assert(foos.getAll("id", 1, 2), {
      a: Seq[SelectFoo] => a.size == 2
    })
  }

  test("select filter") {


    var results = foos.filter(Map("id" -> 1))
    assert(results, {
      f: Seq[SelectFoo] => f.size == 1
    })

    results = foos.filter((f: Var) => f \ "id" > 10)
    assert(results, {
      s: Seq[SelectFoo] => s.size == 40
    })

    results = foos.filter((f: Var) => f.hasFields("id"))
    assert(results, {
      s: Seq[SelectFoo] => s.size == 50
    })


  }
  lazy val foos = table.asInstanceOf[Table[SelectFoo]]
}
        


