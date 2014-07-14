package com.rethinkscala

import com.rethinkscala.ast.{ProduceFloat, ProduceTypedNumeric, ProduceNumeric}
import org.scalatest.FunSuite
import Blocking._

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 7/13/14
 * Time: 6:04 PM
 *
 */
class MathAndLogicTest extends FunSuite with WithBase{


  def check(ast:ProduceNumeric,f:Double=>Boolean)(implicit mf:Manifest[Double]) = assert(ast.run,f)

  def checkf(ast:ProduceFloat,f:Float=>Boolean)(implicit mf:Manifest[Float]) = assert(ast.run,f)


  test("add"){
    val add = Expr(1) + 1
    check(add,_ == 2)
  }

  test("sub"){
    val sub = Expr(1)-1
    check(sub,_==0)
  }
  test("mul"){
    val mul = Expr(2) * 6
    check(mul,_ == 12)

  }
  test("div"){
    val div = Expr(10) / 2
    check(div,_ == 5)
  }
  test("mod"){
    val mod = Expr(12) % 5
    check(mod, _ == 2)
  }
  test("and"){
    val and = Expr(1) && 3
    assert(and,true)


  }
  test("or"){
    val or = Expr(1) || 2
    assert(or, true)
  }

  test("eq"){
    val eq = Expr(1) === 1
    assert(eq, true)
  }
  test("ne"){
    val ne = Expr(1) =!= 2
    assert(ne,true)
  }
  test("gt"){
    val gt = Expr(6) > 4
    assert(gt, true)
  }
  test("ge"){
    val ge = Expr(6) >= 4
    assert(ge,true)
  }
  test("lt"){
    val lt = Expr(7) < 9
    assert(lt,true)
  }
  test("le"){
   val le = Expr(7)<= 9
    assert(le,true)
  }

  test("not"){
    val not = ~Expr(true)
    assert(not,false)

  }

  test("random"){


   check(r.random, _ < 1)
    check(r.random(100), _ < 100)
    check(r.random(10,20), c=> c >10 && c < 20)
    checkf(r.random(1.59, -2.24).toFloat,c=> c> -2.24 && c<1.59)
  }

}
