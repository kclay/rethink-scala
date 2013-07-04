package com.rethinkscala

import com.rethinkscala.ast.{Typed, Var}


/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 7/4/13
 * Time: 10:58 AM
 *
 */

class LiftedTest {


  trait Doc {

    var __scope__ : Var

    def __enter__(implicit v: Var) = __scope__ = v

    def __exit__ = _



  }
  case class Bar(foo:Boolean)

  def map[T](f:T=>Any) = 1

  def a = {

    map[Bar]((b:Bar)=> b.foo)
  }

  /*
case class Foo(a:Int,b:Int) extends Doc{
  def a():ast.Numeric= __scope__ field "a"
}   */


}
