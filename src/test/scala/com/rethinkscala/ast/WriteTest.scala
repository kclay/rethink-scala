package com.rethinkscala.ast

import org.scalatest.FunSuite
import com.rethinkscala.{UpdateResult, InsertResult, Document, BaseTest}
import com.rethinkscala.Implicits._

/** Created with IntelliJ IDEA.
  * User: keyston
  * Date: 6/29/13
  * Time: 5:34 PM
  *
  */

case class Foo(id: String, a: Int, b: Int) extends Document

class WriteTest extends FunSuite with BaseTest {


  test("insert documents") {


    assert(table.insert(Foo("a", 1, 1)), {
      rs: InsertResult => rs.inserted == 1
    })

    assert(table.insert(
      Seq(
        Foo("b", 1, 2), Foo("c", 1, 2))
    ), {
      rs: InsertResult => rs.inserted == 2
    })

  }

  test("updating data") {

    def fetch = table.get("a")
    var update = fetch.update(Map("a" -> 2, "b" -> 2))

    assert(update, {
      u: UpdateResult => u.replaced == 1
    })

    assertAs[Foo](fetch, {
      f: Foo => f.id == "a" && f.a == 2 && f.b == 2
    })

    // TODO fix api so results of function are automaticly converted to Typed
    update = fetch.update((v: Var) => MakeObj(Map("a"->v.attr("a").add(3))))

    assert(update, {
      u: UpdateResult => u.replaced == 1
    })

    assertAs[Foo](fetch, {
      f: Foo => f.a == 5
    })


  }

  test("replace data") {

    // val replace = table.get("a").replace(Foo("a",29,50))
  }

}
