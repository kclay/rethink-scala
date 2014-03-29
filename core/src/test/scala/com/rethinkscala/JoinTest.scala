package com.rethinkscala

import org.scalatest.FunSuite
import scala.Some
import com.rethinkscala.ast.Table
import Blocking._

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 8/17/13
 * Time: 2:12 PM
 *
 */

case class FooJ(id: Int, value: Int) extends Document

case class BarJ(id: Int, value: Int) extends Document

class JoinTest extends FunSuite with WithBase {


  type R = JoinResult[FooJ, BarJ]

  val foos = Table[FooJ]("foo", db = Some(db))
  val bars = Table[BarJ]("bar", db = Some(db))


  test("eq join ") {


    // println(foos.run)

    val join = foos.eqJoin("value", bars)



    val results = join.toOpt





    assert(results.isDefined)

    assert(results.get.size == 1)
    val head = results.get.head
    assert(head.left == FooJ(1, 1) && head.right == BarJ(1, 1))


  }

  test("inner join") {
    val join = foos.innerJoin(bars, (f: Var, b: Var) => f \ "value" === b \ "value")



    assert(join.run, {
      f: Iterable[R] => f.size == 1 && f.head.left == FooJ(1, 1)
    })
  }

  test("outer join") {
    val join = foos.outerJoin(bars, (f: Var, b: Var) => f \ "value" === b \ "value")

    assert(join, {
      f: Iterable[R] => f.size == 1 && f.head.left == FooJ(1, 1)
    })
  }

  test("zip join") {
    val join = foos.innerJoin(bars, (f: Var, b: Var) => f \ "value" === b \ "value")
    assert(join.zip.run, {
      rs: Iterable[ZipResult[FooJ, BarJ]] => rs.size == 1 && rs.head.left.isInstanceOf[FooJ] && rs.head.right.isInstanceOf[BarJ]
    })
  }

  override protected def beforeAll() {
    super.beforeAll()
    foos.create.run
    bars.create.run

    foos.insert(FooJ(1, value = 1)).run
    bars.insert(BarJ(1, value = 1)).run
  }
}
