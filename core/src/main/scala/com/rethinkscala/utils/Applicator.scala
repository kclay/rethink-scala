package com.rethinkscala.utils

import com.rethinkscala.ast.{Typed, Var}

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 10/31/13
 * Time: 12:41 PM 
 */

trait Applicator[T, +R] {
  self =>


  implicit def varToCast(v: Var): T = v.asInstanceOf[T]

  type Method
  var applicator: Method = _

  type PMethod = PartialFunction[Method, R]

  def <~(op: Method) = apply0(op)

  def apply(op: PMethod) = _

  def !(op: Method) = apply0(op)

  def apply0(op: Method): R = {
    applicator = op
    apply
  }

  // var applicator: Method = _

  /* def lift(op: Method): R = {
     applicator = op
     apply
   }  */

  def apply: R
}

abstract class Applicator1[T, R] extends Applicator[T, R] with Function1[Var, Typed] {
  self =>

  type Method = (T) => Typed


  def apply(v1: Var) = applicator(v1)
}

abstract class Applicator2[T, R] extends Applicator[T, R] with Function2[Var, Var, Typed] {
  self =>
  type Method = (T, T) => Typed

  def apply(v1: Var, v2: Var) = applicator(v1, v2)


}