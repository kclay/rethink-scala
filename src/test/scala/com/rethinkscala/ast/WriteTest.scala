package com.rethinkscala.ast

import org.scalatest.FunSuite
import com.rethinkscala.{ InsertResult, Document, BaseTest }

/** Created with IntelliJ IDEA.
 *  User: keyston
 *  Date: 6/29/13
 *  Time: 5:34 PM
 *
 */
class WriteTest extends FunSuite with BaseTest {

  test("insert documents") {

    case class Foo(a: Int, b: Int) extends Document

    assert(table.insert(Foo(1, 1)), {
      rs: InsertResult => rs.inserted == 1
    })

    assert(table.insert(
      Seq(
        Foo(1, 2), Foo(1, 2))
    ), {
      rs: InsertResult => rs.inserted == 2
    })

  }

}
