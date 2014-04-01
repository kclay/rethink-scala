package com.rethinkscala.magnets

import com.rethinkscala.{FromAst, ImplicitConversions, FilterTyped}
import com.rethinkscala.ast.{Produce0, Var, Typed}

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 3/26/2014
 * Time: 2:06 PM 
 */


trait ToNameReceptaclePimps {
  implicit def symbol2NR(symbol: Symbol) = new FieldFilterReceptacle[String](symbol.name)

  implicit def string2NR(string: String) = new FieldFilterReceptacle[String](string)



  implicit def func2NR[P](f: Var => Produce0[P])= new FunctionFilterReceptacle[P](f)
}




trait FilterReceptacle[T] {

  def apply: FilterTyped

  def as[B]:FilterReceptacle[B]

}

case class FunctionFilterReceptacle[T](f: Var => Typed) extends FilterReceptacle[T] {


  def apply = f


  def as[B] = FunctionFilterReceptacle[B](f)
}

case class FieldFilterReceptacle[T](name: String) extends FilterReceptacle[T] {


  def apply = name

  def as[B] = FieldFilterReceptacle[B](name)
}

