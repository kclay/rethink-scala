package com.rethinkdb

import com.rethinkdb.ast._
import com.rethinkdb.ast.Desc
import com.rethinkdb.ast.SliceRange
import com.rethinkdb.ast.Asc
import com.rethinkdb.ast.Var

/** Created with IntelliJ IDEA.
 *  User: keyston
 *  Date: 5/30/13
 *  Time: 6:49 PM
 *  To change this template use File | Settings | File Templates.
 */
object Implicits {

  case class String2Ast(name: String) {
    def row = r.row(name)

    def asc = Asc(name)

    def desc = Desc(name)
  }

  implicit def string2Ast(name: String) = String2Ast(name)

  implicit def string2Ordering(name: String) = Asc(name)

  implicit def intWithTildyArrow(i: Int) = new {
    def ~>(j: Int) = SliceRange(i, j)
  }
  implicit def toPredicate1(f: (Var) => Typed) = new Predicate1(f)
  implicit def toPredicate2(f: (Var, Var) => Typed) = new Predicate2(f)

}
