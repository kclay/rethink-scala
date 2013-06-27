package com.rethinkdscala.ast

import org.scalatest.FunSuite

import com.rethinkscala._
import com.rethinkscala.ast._
import ql2.{ Ql2 => p }
import com.rethinkdscala.BaseTest

class MappingTest extends FunSuite with BaseTest {

  test("should return TableResult") {

    val db = DB("test")

    assert(db.tableCreate("bar"), true)

    val info = db.table("bar").info

    assertAs[TableInfoResult](info, {
      i: TableInfoResult => i.name == "bar" && i.kind == "TABLE" && i.db.name == "test"
    })
  }

}
