package com.rethinkscala

import org.scalatest.FunSuite


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

    assert(foos.between(10, 20, BetweenOptions(leftBound = Some(Bound.Closed), rightBound = Some(Bound.Closed))).orderBy("id"), {
      f: Seq[SelectFoo] => f.size == 11 & f.last.id == 20
    })

    assert(foos.between(10, 20, BetweenOptions(leftBound = Some(Bound.Open), rightBound = Some(Bound.Closed))).orderBy("id"), {
      f: Seq[SelectFoo] => f.size == 10 & f.last.id == 20 && f(0).id == 11
    })


  }

  test("table select") {
    assert(foos, {
      f: Seq[SelectFoo] => f.size == 50
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

    results = foos.filter(f => f \ "id" > 10)
    assert(results, {
      s: Seq[SelectFoo] => s.size == 40
    })

    results = foos.filter(f => f.hasFields("id"))
    assert(results, {
      s: Seq[SelectFoo] => s.size == 50
    })


  }


  class ListHelper[T](ls: List[T]) {
    /** @param size  The size of each sub list */
    def chunk(size: Int) = List.range(0, ls.size, size).map { i => ls.slice(i, i + size)}
  }

  implicit def list2helper[T](ls: List[T]) = new ListHelper(ls)

  test("5000 results") {


    val chunks = List.range(0, 50000).chunk(125)


    chunks.map {
      chunk =>
        foos.insert(chunk.map(SelectFoo(_))).run
    }

    val length = foos.count().run

    foos.orderBy("id").toOpt.map {
      case seq =>
        val len = seq.length
        val c = seq.chunks
        val b = c.size
        seq

    }

  }


  lazy val foos = table.to[SelectFoo]
}
        


