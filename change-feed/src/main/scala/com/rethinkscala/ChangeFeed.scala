package com.rethinkscala

import com.rethinkscala.ast.{Table, Changes}

import com.rethinkscala.Document

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 8/16/14
 * Time: 8:41 PM
 *
 */
class TableChangeFeed[T<:Document](val table:Table[T]) extends AnyVal{
  def changes = Changes(table)
}
object ChangeFeed {


  implicit  def tableToChangeFeed[T<:Document](table:Table[T]) = new TableChangeFeed(table)

}
