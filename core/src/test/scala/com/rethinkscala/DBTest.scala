package com.rethinkscala

import com.rethinkscala.Blocking._
import com.rethinkscala.net.ProtoBufCompiledAst
import org.scalatest.FunSuite
import ql2.Ql2.Term

class DBTest extends FunSuite with WithBase {


  test("create db instance") {

    val query = r.dbCreate("foo")
    val ProtoBufCompiledAst(ast) = r.dbCreate("foo").ast

    assert(ast, Term.TermType.DB_CREATE)
    assert(ast.getArgsCount == 1)
    assert(ast.getArgs(0), Term.TermType.DATUM)
    assert(ast.getArgs(0).getDatum.getRStr == "foo")

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

  override def useVersion = version2
}