package com.rethinkscala.lifted

import language.experimental.macros
import scala.reflect.macros.Context

import com.rethinkscala.ast.{Var, Predicate1, Typed}

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 7/4/13
 * Time: 3:03 PM 
 */


object LiftedPredicate {

  def apply[T, R](f: T => R): Predicate1 = macro impl[T, R]

  def impl[T: c.WeakTypeTag, R: c.WeakTypeTag](c: Context)(f: c.Expr[T => R]) = {
    import c.universe._
    import com.rethinkscala.Implicits._


    reify {
      new Predicate1((v: Var) => v.add(1))
    }

  }
}


