package com.rethinkscala

import org.scalatest.FunSuite

import com.rethinkscala.Implicits.Quick._
import com.rethinkscala.ast._
import com.rethinkscala.ast.StringDatum
import com.rethinkscala.ast.NumberDatum
import Blocking._
import com.rethinkscala.net.ProtoBufCompiledAst

case class ExprCase(value:List[String]=List.empty,id:Option[String] = None)
class ExprTest extends FunSuite with WithBase {

  /*
  import scala.collection.JavaConverters._

  test("auto casting") {

    assert(Expr(1).isInstanceOf[NumberDatum])
    assert(Expr(1L).isInstanceOf[NumberDatum])
    assert(Expr("hello").isInstanceOf[StringDatum])
    assert(Expr(Seq(1, 2, 3)).isInstanceOf[MakeArray[Int]])
    assert(Expr(Map.empty[String, Boolean]).isInstanceOf[MakeObj])

  }

  test("map conversion") {
    val map = Seq(("foo", "bar"), ("bar", 1), ("you", Seq(1, 2, 3))).toMap

    val ProtoBufCompiledAst(objTerm) = Expr(map).ast

    val optargs = objTerm.getOptargsList asScala

    assert(optargs.size == 3)
    assert(optargs(0).key.get == "foo")
    assert(optargs(0).str.get == "bar")

    assert(optargs(1).key.get == "bar")
    assert(optargs(1).num.get == 1)
    assert(optargs(2).key.get == "you")

    val array = optargs(2).array

    array.left.get.zipWithIndex foreach {
      e => assert(e._1.num.get == e._2 + 1)
    }
  }
  */

  test("allow the use of r.row inside an update context and other ImplictVar"){

    val results = table.to[ExprCase].insert(ExprCase()).toOpt.flatMap{
      ir=>
        val id = ir.generatedKeys.head
        table.to[ExprCase].get(id).update(Map("value" -> r.row("value").append("foo"))).withChanges.toOpt
    }

    assert(results.flatMap(c=> c.returnedValue[ExprCase]).map(_.value == List("foo")).getOrElse(false))





  }

  //override def setupDB = false

  override def useVersion = version3
}
