package com.rethinkscala

import org.scalatest.FunSuite

import com.rethinkscala.ast.{Expr, Var, Sequence, Typed}
import com.rethinkscala.Blocking._

class TransformationTest extends FunSuite {

  test("test map") {


      //https://issues.scala-lang.org/browse/SI-6221
    //https://github.com/scala/scala/pull/2650

    val term = r.table[Document]("marvel").map(hero =>Expr(Seq(hero.as[Int]("a"),hero.as[Int]("b"))))





    r.table[Document]("marvel").map(hero=> Expr(Seq(1,2)))


















    // ! ((hero: Var) => hero \ "combatPower" + hero \ "combatPower" * 2)

    //println(ast)

  }
}
