package com.rethinkscala.ast

import org.scalatest.FunSuite

import com.rethinkscala._
import ql2.{ Ql2 => p }

class MappingTest extends FunSuite with BaseTest {

  after {
    r.table(tableName).drop.run
  }
  test("should return TableResult") {

    val table = r.table(tableName)

    assert(table.create, true)

    assertAs[TableInfoResult](table.info, {
      i: TableInfoResult => i.name == tableName && i.kind == "TABLE" && i.db.name == "test"

    })
    //table.drop.run
  }

}
