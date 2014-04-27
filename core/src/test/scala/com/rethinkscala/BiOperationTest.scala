package com.rethinkscala

import org.scalatest.FunSuite

import com.rethinkscala._
import ast._
import com.rethinkscala.net.ProtoBufCompiledAst

class BiOperationTest extends FunSuite with WithBase {


  import scala.collection.JavaConverters._


  test("TermNode.add") {


    val addNum = Expr(1) + 2

    val ProtoBufCompiledAst(term) = addNum.ast
    var args = term.getArgsList.asScala

    assert(addNum.isInstanceOf[Add])
    assert(args.size == 2)
    assert(args(0).getDatum.getRNum == 1.0)
    assert(args(1).getDatum.getRNum == 2.0)

    val addStr = Expr("hello") += "world"

    val ProtoBufCompiledAst(term2) = addStr.ast
    args = term2.getArgsList.asScala
    assert(args.size == 2)
    assert(args(0).getDatum.getRStr == "hello")
    assert(args(1).getDatum.getRStr == "world")

  }

  override def setupDB = false
}
