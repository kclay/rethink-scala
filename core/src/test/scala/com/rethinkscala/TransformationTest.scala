package com.rethinkscala

import org.scalatest.FunSuite

import com.rethinkscala._
import com.rethinkscala.Implicits._
import com.rethinkscala.ast.Var

class TransformationTest extends FunSuite {

  test("test map") {
    val term = r.table("marvel").map ! ((hero: Var) => hero \ "combatPower" + hero \ "combatPower" * 2)

    val ast = term.ast
    assert(true)
    //println(ast)

  }
}
