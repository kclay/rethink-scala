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


  def <~(op: Method) = apply0(op)


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
  type PMethod = PartialFunction[(T), Typed]
  var applicator2: Option[PMethod] = None

  def apply(op: PMethod): R = {
    applicator2 = Some(op)
    apply
  }

  def apply(v1: Var): Typed = applicator2.map(f => f.apply(v1)).getOrElse(applicator(v1))
}

abstract class Applicator2[T, R] extends Applicator[T, R] with Function2[Var, Var, Typed] {
  self =>
  type Method = (T, T) => Typed

  type PMethod = PartialFunction[(T, T), Typed]


  var applicator2: Option[PMethod] = None

  def apply(op: PMethod): R = {
    applicator2 = Some(op)
    apply
  }

  def apply(v1: Var, v2: Var): Typed = applicator2.map(f => f.apply((v1, v2))).getOrElse(applicator(v1, v2))


}