package com.rethinkdb

import com.rethinkdb.ast.{PRange, Asc}

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 5/30/13
 * Time: 6:49 PM
 * To change this template use File | Settings | File Templates.
 */
object Implicits {

  implicit def string2Ordering(name:String)=Asc(name)
  implicit def intWithTildyArrow(i: Int) = new {
    def ~>(j: Int) = PRange(i, j)
  }
}
