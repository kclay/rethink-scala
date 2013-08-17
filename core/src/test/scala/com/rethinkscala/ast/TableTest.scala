package com.rethinkscala.ast

import org.scalatest.FunSuite
import com.rethinkscala.WithBase
import com.rethinkscala._
import com.rethinkscala.Implicits._

/** Created with IntelliJ IDEA.
 *  User: keyston
 *  Date: 6/27/13
 *  Time: 4:33 PM
 *  To change this template use File | Settings | File Templates.
 */
class TableTest extends FunSuite with WithBase {

  test("create table with options") {

    val table = r.table(tableName)
    val create = table.create(TableOptions(primaryKey=Some("a"), durability=Some(Durability.Hard), cacheSize=Some(16)))

    assert(create)

  }
  test("list tables") {

    assert[IS](r.tables, {
      t: IS => t.toSeq.contains(tableName)
    })
  }
  test("add secondary index to table") {

    val table = r.table(tableName)

    val index = table.indexCreate("foo_count", (bar: Var) => bar \ "foos" + 2 * bar \ "foos")

    assert(index)

  }

  test("list index") {

    val table = r.table(tableName)
    assert[IS](table.indexes, {
      i: IS => {
        val seq = i.toSeq
        seq.contains("foo_count")
      }
    })
  }

  test("drop index") {
    val table = r.table(tableName)
    assert(table.indexDrop("foo_count"))
  }

  test("drop table") {
    assert(r.table(tableName).drop)
  }

  override def setupDB = false
}
