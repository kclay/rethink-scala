package com.rethinkscala.japi

import com.rethinkscala.ast.{Predicate => SPredicate, BooleanPredicate => BP, Typed, Var, Binary}

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 9/13/13
 * Time: 2:09 AM
 *
 */
/**
 * A Function interface. Used to create first-class-functions is Java.
 */
trait Function[T, R] {
  @throws(classOf[Exception])
  def apply(param: T): R

}

/**
 * A Function interface. Used to create 2-arg first-class-functions is Java.
 */
trait Function2[T1, T2, R] {

  @throws(classOf[Exception])
  def apply(arg1: T1, arg2: T2): R
}



trait Predicate extends SPredicate with Function[Var, Typed] {
  protected def _invoke(vars: Seq[Var]) = apply(vars(0))

  val amount: Int = 1
}

trait Predicate2 extends SPredicate with Function2[Var, Var, Typed] {
  protected def _invoke(vars: Seq[Var]) = apply(vars(0), vars(1))

  val amount: Int = 2
}


trait BooleanFunction extends  BP with Function[Var, Binary] {
  protected def _invoke(vars: Seq[Var]) = apply(vars(0))

  val amount: Int = 1

}


trait BooleanFunction2 extends BP with Function2[Var, Var, Binary]  {
  protected def _invoke(vars: Seq[Var]) = apply(vars(0), vars(1))

  val amount: Int = 2
}