package com.rethinkdb

import com.rethinkdb.ast.{ ImplicitVar, Table }

/** Created with IntelliJ IDEA.
 *  User: keyston
 *  Date: 5/28/13
 *  Time: 5:53 PM
 *  To change this template use File | Settings | File Templates.
 */
object r {

  private lazy val _row = new ImplicitVar

  def row(name: String) = _row \ name
  def table(name: String, useOutDated: Option[Boolean] = None) = Table(name, useOutDated)
}