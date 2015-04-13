package com.rethinkscala

import com.rethinkscala.ast.{Asc, BooleanDatum, Branch, ByAvg, BySum, DB, DBCreate, DBDrop, DBList, Desc, FuncCall, JavaScript, NumberDatum, ScalaBooleanPredicate1, StringDatum, Table, TableCreate, UserError, _}
import org.joda.time.{DateTime, ReadableInstant}

import scala.collection.Iterable


/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 9/12/13
 * Time: 3:14 PM
 *
 */


trait RethinkApi extends TimeNames with GeometryApi {
  def getInstance = this

  private lazy val _row = new ImplicitVar

  def expr(term: Term): Term = term

  def expr(value: String): StringDatum = Expr(value)

  def expr(value: Boolean): BooleanDatum = Expr(value)

  def expr(value: Int): NumberDatum = Expr(value)

  def expr(value: Long): NumberDatum = Expr(value)

  def expr(value: Float): NumberDatum = Expr(value)

  def expr(value: Double): NumberDatum = Expr(value)

  def expr(value: Document) = Expr(value)

  def expr(value: ReadableInstant) = Expr(value)

  def expr(v: Any) = Expr(v)

  def expr[T](value: Iterable[T]) = Expr(value)

  // TODO Fix me, clashes with Sequence and Hash
  def row: ImplicitVar = _row

  def string = row.string

  lazy val test = db("test")

  def table(name: String): Table[Document] = table(name, None)

  def table(name: String, useOutDated: Boolean): Table[Document] = table(name, Some(useOutDated))

  def table(name: String, useOutDated: Option[Boolean] = None): Table[Document] = Table[Document](name, useOutDated)

  def tableAs[T <: AnyRef](name: String): Table[T] = tableAs[T](name, None)

  def tableAs[T <: AnyRef](name: String, useOutDated: Boolean): Table[T] = tableAs[T](name, Some(useOutDated))

  def tableAs[T <: AnyRef](name: String, useOutDated: Option[Boolean] = None): Table[T] = Table[T](name, useOutDated)

  def db(name: String) = DB(name)

  def dbCreate(name: String) = DBCreate(name)

  def dbDrop(name: String) = DBDrop(name)

  def dbs = DBList()

  def tableCreate(name: String): TableCreate = tableCreate(name, None)

  def tableCreate(name: String, options: TableOptions): TableCreate = tableCreate(name, Some(options))

  def tableCreate(name: String, options: Option[TableOptions] = None): TableCreate = test.tableCreate(name, options.getOrElse(TableOptions()))

  def tableDrop(name: String): TableDrop = test.tableDrop(name)

  def tables: TableList = test.tables

  def branch(predicate: (Var) => Binary, passed: Typed, failed: Typed): Branch = branch(ScalaBooleanPredicate1(predicate), passed, failed)

  def branch(predicate: Binary, passed: Typed, failed: Typed): Branch = Branch(predicate, passed, failed)

  @deprecated("Call .sum on collection")
  def sum(attr: String): BySum = BySum(attr)

  @deprecated("Call .avg on collection")
  def avg(attr: String): ByAvg = ByAvg(attr)

  def random = Random[Double](Seq.empty)

  def random[@specialized(Int, Double, Long) T](values: T*) = Random[T](values)

  // def random(values:Float*) = Random[Float,Float](values,true)

  val count = ByCount

  def asc(attr: String): Asc = Asc(attr.wrap)

  def asc(attr: Typed): Asc = Asc(attr.wrap)

  def desc(attr: String): Desc = Desc(attr.wrap)

  def desc(attr: Typed): Desc = Desc(attr.wrap)

  private[this] def compute[T](v: T*)(a: (T, T) => T) = v.drop(1).foldLeft(v.head)(a)

  def sub(v: Numeric*) = compute[Numeric](v: _*)(_ - _)

  def call(f: Predicate, values: Typed*) = FuncCall(f, values)

  def error(msg: String) = UserError(msg)

  def js(code: String, timeout: Option[Int] = None) = JavaScript(code, timeout)

  def json(str: String) = Json(str)

  def uuid = new ast.UUID()


}


trait TimeNames {

  import com.rethinkscala.ast.TimeName
  import ql2.Ql2.Term.TermType

  private def apply(tt: TermType) = TimeName(tt)

  val monday = this(TermType.MONDAY)
  val tuesday = this(TermType.TUESDAY)
  val wednesday = this(TermType.WEDNESDAY)
  val thursday = this(TermType.THURSDAY)
  val friday = this(TermType.FRIDAY)
  val saturday = this(TermType.SATURDAY)
  val sunday = this(TermType.SUNDAY)
  val weekdays = Seq(monday, tuesday, wednesday, thursday, friday, saturday, sunday)

  val january = this(TermType.JANUARY)
  val february = this(TermType.FEBRUARY)
  val march = this(TermType.MARCH)
  val april = this(TermType.APRIL)
  val may = this(TermType.MAY)
  val june = this(TermType.JUNE)
  val july = this(TermType.JULY)
  val august = this(TermType.AUGUST)
  val september = this(TermType.SEPTEMBER)

  val october = this(TermType.OCTOBER)
  val november = this(TermType.NOVEMBER)
  val december = this(TermType.DECEMBER)

  val months = Seq(january, february, march, april, june, july, august, september, october, november, december)

  def weekday(dt: DateTime) = weekdays(dt.dayOfWeek().get() - 1)

  def month(dt: DateTime) = months(dt.monthOfYear().get())

  def now = new Now()
}
