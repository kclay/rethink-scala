package com.rethinkdb

import com.rethinkdb.ast.{Desc, PRange, Asc}

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 5/30/13
 * Time: 6:49 PM
 * To change this template use File | Settings | File Templates.
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
    def ~>(j: Int) = PRange(i, j)
  }


}
