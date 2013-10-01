package com.rethinkscala

import com.rethinkscala.ast._


/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 9/12/13
 * Time: 3:14 PM
 *
 */


object r {

  def getInstance = this
  private lazy val _row = new ImplicitVar


  // TODO Fix me, clashes with Sequence and Hash
  def row(name: String) = (_row: ProduceAny) field name

  //def row[T<:Sequence](name: String)(implicit d:DummyImplicit) = _row.asInstanceOf[T] field name


  def table[T<:Document](name:String):Table[T] = table[T](name,None)

  def table[T<:Document](name:String,useOutDated:Boolean):Table[T] = table[T](name,Some(useOutDated))

  def table[T <: Document](name: String, useOutDated: Option[Boolean] = None):Table[T] = Table[T](name, useOutDated)

  def db(name: String) = DB(name)

  def dbCreate(name: String) = DBCreate(name)

  def dbDrop(name: String) = DBDrop(name)

  def dbs = DBList()


  def tableCreate(name:String):TableCreate=tableCreate(name,None)
  def tableCreate(name:String,options:TableOptions):TableCreate=tableCreate(name,Some(options))
  def tableCreate(name: String,options:Option[TableOptions]=None):TableCreate = TableCreate(name, options.getOrElse(TableOptions()))

  def tableDrop(name: String) = TableDrop(name)

  def tables = TableList()


  def branch(predicate:(Var)=>Binary,passed:Typed,failed:Typed):Branch = branch(BooleanPredicate1(predicate),passed,failed)
  def branch(predicate: Binary, passed: Typed, failed: Typed):Branch = Branch(predicate, passed, failed)

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
