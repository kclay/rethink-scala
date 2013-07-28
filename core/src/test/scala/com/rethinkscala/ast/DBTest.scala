package com.rethinkscala.ast

import org.scalatest.FunSuite

import com.rethinkscala._
import ql2._
import com.rethinkscala.WithBase

class DBTest extends FunSuite with WithBase {

  test("create db instance") {

    val query = r.dbCreate("foo")
    val ast = r.dbCreate("foo").ast

    assert(ast, Term.TermType.DB_CREATE)
    assert(ast.`args`.size == 1)
    assert(ast.`args`(0), Term.TermType.DATUM)
    assert(ast.`args`(0).`datum`, "foo")

    assert(query, true)

  }
  test("list database") {

    assert[IS](r.dbs, {
      x: IS => x.toSeq.contains("foo")
    })
  }

  test("drop db instance") {

    val query = r.dbDrop("foo")

    assert(query, true)
  }

  /*
  test("create db instance"){
   val db = DB("foo")

    val term = db.toTerm

    assert(term.getType == TermType.DB)
    assert(term.getArgsCount == 1)
    assert(term.getArgs(0).getType == TermType.DATUM)
    assert(term.getArgs(0).getDatum.getRStr=="foo")

  }
  test("db with table"){


    val db = DB("foo")
    val term = db.table("bar",true).toTerm
     System.out.println(term)
    assert(term.getType ==TermType.TABLE)
    assert(term.getArgsCount == 2)
    assert(term.getArgs(0).getType == TermType.DB)
    assert(term.getArgs(0).getArgs(0).getDatum.getRStr=="foo")

    assert(term.getArgs(1).getType == TermType.DATUM)
    assert(term.getArgs(1).getDatum.getRStr=="bar")

    assert(term.getOptargsCount==1)
    assert(term.getOptargs(0).getKey=="use_outdated")
    assert(term.getOptargs(0).getVal.getDatum.getRBool == true)
   // asser(term.getArgsCount)
  } */
}