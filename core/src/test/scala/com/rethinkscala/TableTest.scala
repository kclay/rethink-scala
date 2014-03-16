package com.rethinkscala

import org.scalatest.FunSuite
import com.rethinkscala.ast.Var
import Blocking._

/** Created with IntelliJ IDEA.
  * User: keyston
  * Date: 6/27/13
  * Time: 4:33 PM
  * To change this template use File | Settings | File Templates.
  */
class TableTest extends FunSuite with WithBase {
  test("create table with options") {

    val table = r.db("test").table(tableName)
    val create = table.create(TableOptions(primaryKey = Some("a"), durability = Some(Durability.Hard), cacheSize = Some(16)))

    val ast = create.ast
    assert(create)

  }
  test("list tables") {


    assert(r.tables.run, {
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
    assert(table.indexes.run, {
      i: IS => {
        val seq = i.toSeq
        seq.contains("foo_count")
      }
    })
  }

  test("index status") {
    val table = r.table(tableName)


    assert(table.indexStatus(), {
      i: Seq[IndexStatusResult] => {
        i.size == 1
      }
    })
  }

  test("index wait") {
    val table = r.table(tableName)
    table.indexCreate("hello").run
    assert(table.indexWait() ,{
      i:Seq[IndexStatusResult]=> i.size == 2
    })
    assert(table.indexWait("hello"),{
      i:Seq[IndexStatusResult]=> i.size == 1
    })



  }



  test("insert into table") {


    val table = r.table(tableName)
    val insert = table.insertMap(Seq(Map("a" -> 1, "b" -> 2)))
    assert(insert.run, {
      i: InsertResult => i.inserted == 1
    })

  }

  test("get") {
    val table = r.table(tableName)
    val record = Map("a" -> 2, "b" -> 2)
    val doc = table.insertMap(Seq(record)).toOpt.map(i => table.get(2).toOpt).flatten


    doc.map(_.toMap) should equal(Some(record))



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
