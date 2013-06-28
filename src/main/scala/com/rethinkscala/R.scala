package com.rethinkscala

import com.rethinkscala.ast._
import com.rethinkscala.ast.Table
import com.rethinkscala.ast.DBCreate
import com.rethinkscala.ast.DB

object r {

  private lazy val _row = new ImplicitVar

  def row(name: String) = _row \ name

  def table(name: String, useOutDated: Option[Boolean] = None) = Table(name, useOutDated)

  def db(name: String) = DB(name)

  def dbCreate(name: String) = DBCreate(name)

  def dbDrop(name: String) = DBDrop(name)

  def dbs = DBList()

  def tableCreate(name: String, primaryKey: Option[String] = None,
                  cacheSize: Option[Int] = None,
                  durability: Option[String] = None, dataCenter: Option[String] = None) = {
    TableCreate(name, primaryKey, cacheSize, durability, dataCenter)
  }

  def tableDrop(name: String) = TableDrop(name)

  def tables = TableList()

  def branch(predicate: BooleanPredicate, passed: Typed, failed: Typed) = Branch(predicate, passed, failed)

  def sum(attr: String) = BySum(attr)

  def avg(attr: String) = ByAvg(attr)

  val count = ByCount

  def asc(attr: String) = Asc(attr)

  def desc(attr: String) = Desc(attr)

  private[this] def compute[T](v: T*)(a: (T, T) => T) = v.drop(1).foldLeft(v.head)(a)

  // def eq(base:Comparable,v:Comparable *) = compute[Comparable](v:_*)()
  def add(v: Addition*) = compute[Addition](v: _*)(_ + _)

  def sub(v: Numeric*) = compute[Numeric](v: _*)(_ - _)

  def call(f: Predicate, values: Typed*) = FuncCall(f, values)
  def error(msg: String) = UserError(msg)

  def js(code: String, timeout: Option[Int] = None) = JavaScript(code, timeout)

}