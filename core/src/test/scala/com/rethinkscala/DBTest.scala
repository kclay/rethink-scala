package com.rethinkscala

import com.rethinkscala.Blocking._
import com.rethinkscala.net.ProtoBufCompiledAst
import org.scalatest.FunSuite
import ql2.Ql2.Term

class DBTest extends FunSuite with WithBase {


  test("create db instance") {

    val query = r.dbCreate("foo")



    assert(query, true)

  }
  test("list database") {


    assert(r.dbs.run, {
      x: IS => x.toSeq.contains("foo")
    })
  }

  test("drop db instance") {

    val query = r.dbDrop("foo")

    assert(query, true)
  }


}