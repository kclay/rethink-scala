package com.rethinkscala.magnets

import com.rethinkscala.{FromAst, ImplicitConversions, FilterTyped}
import com.rethinkscala.ast.{Var, Typed}

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 3/26/2014
 * Time: 2:06 PM 
 */


trait ToNameReceptaclePimps {
  implicit def symbol2NR(symbol: Symbol) = new NameReceptacle[String](symbol.name)

  implicit def string2NR(string: String) = new FieldFilterReceptacle[String](string)



  implicit def func2NR[P >: Var](f: Var => P)(implicit fa: FromAst[P]) = new FunctionFilterReceptacle[fa.Raw](f.asInstanceOf[(Var) => Typed])
}

case class NameReceptacle[T](name: String) {
  def as[B] = NameReceptacle[B](name)
}


trait FilterReceptacle[T] {
  type Out


  def apply: FilterTyped


}

case class FunctionFilterReceptacle[T](f: Var => Typed) extends FilterReceptacle[T] {
  type Out = T

  def apply = f


  def as[B] = FunctionFilterReceptacle[B](f)
}

case class FieldFilterReceptacle[T](name: String) extends FilterReceptacle[T] {
  type Out = T

  def apply = name

  def as[B] = FieldFilterReceptacle[B](name)
}

