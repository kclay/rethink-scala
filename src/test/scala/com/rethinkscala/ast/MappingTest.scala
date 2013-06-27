package com.rethinkscala.ast

import org.scalatest.FunSuite

import com.rethinkscala._
import ql2.{Ql2 => p}

class MappingTest extends FunSuite with BaseTest {

  test("should return TableResult") {

    val db = DB("test")

    assert(db.tableCreate("bar"), true)

    val table = db.table("bar")


    assertAs[TableInfoResult](table.info, {
      i: TableInfoResult => i.name == "bar" && i.kind == "TABLE" && i.db.name == "test"
    })
    table.drop
  }

}
