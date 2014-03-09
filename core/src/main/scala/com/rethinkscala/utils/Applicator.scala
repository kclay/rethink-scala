package com.rethinkscala.utils

import com.rethinkscala.ast._
import com.rethinkscala.ToAst
import com.rethinkscala.ast.Reduce
import com.rethinkscala.ast.Var
import scala.Some

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 10/31/13
 * Time: 12:41 PM 
 */


trait Applicator[T, +R, F] {
  self =>


  implicit def varToCast(v: Var): T = v.asInstanceOf[T]

  type Method
  var applicator: Method = _


  def <~(op: Method) = apply(op)


  def !(op: Method) = apply(op)

  def apply(op: Method): R = {
    applicator = op
    _apply
  }

  def view: F

  // var applicator: Method = _

  /* def lift(op: Method): R = {
     applicator = op
     apply
   }  */

  protected def _apply: R
}

abstract class Applicator1[T, R] extends Applicator[T, R, Function1[Var, Typed]] {
  self =>

  type Method = (T) => Typed
  type PMethod = PartialFunction[(T), Typed]
  var applicator2: Option[PMethod] = None
  /*
  def apply(op: PMethod): R = {
    applicator2 = Some(op)
    _apply
  }     */

  def view = (v1) => applicator2.map(f => f.apply(v1)).getOrElse(applicator(v1))

  //def apply(v1: Var): Typed =
}

abstract class Applicator2[T, R] extends Applicator[T, R, Function2[Var, Var, Typed]] {
  self =>
  type Method = (T, T) => Typed

  type PMethod = PartialFunction[(T, T), Typed]


  var applicator2: Option[PMethod] = None

  /*
  def apply(op: PMethod): R = {
    applicator2 = Some(op)
    _apply
  }        */

  def view = (v1, v2) => applicator2.map(f => f.apply((v1, v2))).getOrElse(applicator(v1, v2))


}