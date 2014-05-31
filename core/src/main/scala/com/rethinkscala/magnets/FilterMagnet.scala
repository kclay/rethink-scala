package com.rethinkscala.magnets

import com.rethinkscala.{FilterTyped, FromAst, ImplicitConversions}
import com.rethinkscala.ast.{Produce0, Var, Typed}

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 3/26/2014
 * Time: 2:06 PM 
 */








trait ReceptacleImplicits {

  implicit def symbolToNR(symbol: Symbol)= new FieldReceptacle[String](symbol.name)

  implicit def stringToNR(string: String) = new FieldReceptacle[String](string)
  implicit def funcToNR[P](f: Var => Produce0[P])= new FunctionReceptacle[P](f)
}





trait Receptacle[T,R] {

  def apply:FilterTyped

  def as[B]:Receptacle[B,R]

}

trait FilterReceptacle[T]  extends Receptacle[T,FilterTyped]

case class FunctionReceptacle[T](f: Var => Typed) extends FilterReceptacle[T] {


  def apply = f


  def as[B] = FunctionReceptacle[B](f)
}

case class FieldReceptacle[T](name: String) extends FilterReceptacle[T] {


  def apply = name

  def as[B] = FieldReceptacle[B](name)
}

