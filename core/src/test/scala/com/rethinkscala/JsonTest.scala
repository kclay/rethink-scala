package com.rethinkscala

import org.scalatest.FunSuite
import com.rethinkscala.ast._
import org.joda.time.DateTime
import com.rethinkscala.net.RethinkDriverError
import com.rethinkscala.reflect.Reflector
import com.rethinkscala.ast.LazyJson
import com.rethinkscala.net.RethinkDriverError

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 12/23/13
 * Time: 12:30 PM 
 */
class JsonTest extends FunSuite with WithBase {

  import Implicits.Quick._

  case class IsJsonDocument(a: Boolean, b: String, c: Int) extends Document

  case class NonJsonDocument(a: DateTime) extends Document


  test("json") {

    assert(r.json("[1,2,3]").run, {
      d: JsonDocument => d.toList == Seq(1, 2, 3)
    })

    assert(r.json("1").run, {
      d: JsonDocument => d.toInt == 1
    })

    assert(r.json("{}"), {
      d: JsonDocument => d.raw == "{}"
    })


  }
  test("isJson") {

    assert(!Expr.isJson(r.expr("true")), "Term can't be json")

    assert(Expr.isJson(Map("foo" -> "bar", "1" -> Map("1" -> 2))), "Nested map is json")

    assert(Expr.isJson(Seq("1", 2, 4, 5, "a")), "List is json")


    assert(Expr.isJson(IsJsonDocument(a = true, "foo", 1)), "Document is json")

    assert(!Expr.isJson(NonJsonDocument(DateTime.now())), "Document with DateTime isn't json")

    intercept[RethinkDriverError] {
      Expr.isJson("a", -1)
    }


  }

  test("exprJson") {

    val a = r.expr("true")
    assert(Expr.json(a) == a, "Returned same value if Term")
    val map = Map("foo" -> "bar", "1" -> Map("1" -> 2))
    val json = Expr.json(map)
    assert(json.isInstanceOf[LazyJson], "Returned Json term")
    assert(json.ast.getArgs(0).getDatum.getRStr == Reflector.toJson(map), "Map serialization good")

    assert(Expr.json(Seq(1, 2, 3)).ast.getArgs(0).getDatum.getRStr == "[1,2,3]", "List serialization good")

    val doc = IsJsonDocument(a = true, "foo", 1)
    assert(Expr.json(doc).ast.getArgs(0).datum.str == Reflector.toJson(doc), "Document serialization good")

    val doc2 = Expr.json(NonJsonDocument(DateTime.now()))
    assert(doc2.isInstanceOf[MakeObj2], "Non json document make MakeObj2 Term")

    val doc2Val = doc2.optargs.toSeq(0).value

    assert(doc2Val.isInstanceOf[ISO8601], "Doesn't produce json but produces ISO8601 Term")




  }
}
