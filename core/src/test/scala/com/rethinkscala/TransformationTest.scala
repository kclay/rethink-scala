package com.rethinkscala

import org.scalatest.FunSuite

import com.rethinkscala.ast.{Var, Sequence}

class TransformationTest extends FunSuite {

  test("test map") {

    val b = a.as[Int]("abc")

    val table: Sequence[Document] = r.table("marvel")
    val term = r.table("marvel").map(hero => hero.as[Int]("combatPower") + hero.as[Int]("combatPower") * 2)




    // ! ((hero: Var) => hero \ "combatPower" + hero \ "combatPower" * 2)

    val ast = term.ast
    assert(true)
    //println(ast)

  }
}
