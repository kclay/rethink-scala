package com.rethinkscala.ast

import org.scalatest.FunSuite

import com.rethinkscala._
import ql2.{ Ql2 => p }

class MappingTest extends FunSuite with BaseTest {

  test("should return TableResult") {

    assertAs[InfoResult, TableInfoResult](table.info, {
      i: TableInfoResult => i.name == tableName && i.kind == "TABLE" && i.db.name == "test"

    })
    //table.drop.run
  }

}
