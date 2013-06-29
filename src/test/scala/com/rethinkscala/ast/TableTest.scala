package com.rethinkscala.ast

import org.scalatest.FunSuite
import com.rethinkscala.BaseTest
import com.rethinkscala._
import com.rethinkscala.Implicits._

/** Created with IntelliJ IDEA.
 *  User: keyston
 *  Date: 6/27/13
 *  Time: 4:33 PM
 *  To change this template use File | Settings | File Templates.
 */
class TableTest extends FunSuite with BaseTest {

  after {
    println(s"Dropping $tableName")
    r.table(tableName).drop.run
  }

  test("create table with options") {

    val table = r.table(tableName)
    val create = table.create(Some("a"), Some(Durability.Hard), Some(10))

    assert(create)

  }
  test("add secondary index to table") {

    val table = r.table(tableName)

    table.create.run
    val index = table.indexCreate("foo_count", (bar: Var) => bar \ "foos" + 2 * bar \ "foos")

    assert(index)

  }
}
