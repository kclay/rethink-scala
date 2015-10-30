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


trait DBApi {
  lazy val test = db("test")

  def db(name: String): DB = DB(name)

  def dbCreate(name: String): DBCreate = DBCreate(name)

  def dbDrop(name: String): DBDrop = DBDrop(name)

  def dbs: DBList = DBList()

}

trait TableApi extends DBApi {


  def table(name: String): Table[Document] = table(name, None)

  @deprecated("use table(String,ReadMode.Kind)")
  def table(name: String, useOutDated: Boolean): Table[Document] = table(name)

  @deprecated("use table(String,ReadMode.Kind)")
  def table(name: String, useOutDated: Option[Boolean]): Table[Document] = table(name)

  def table(name: String, readMode: ReadMode.Kind = ReadMode.Single): Table[Document] = Table[Document](name, readMode)

  def tableAs[T <: AnyRef](name: String): Table[T] = tableAs[T](name, ReadMode.Single)

  @deprecated("use tableAs(String,ReadMode.Kind)")
  def tableAs[T <: AnyRef](name: String, useOutDated: Boolean): Table[T] = tableAs[T](name)

  @deprecated("use tableAs(String,ReadMode.Kind)")
  def tableAs[T <: AnyRef](name: String, useOutDated: Option[Boolean]): Table[T] = tableAs[T](name)

  def tableAs[T <: AnyRef](name: String, readMode: ReadMode.Kind): Table[T] = Table[T](name, readMode)


  def tableCreate(name: String): TableCreate = tableCreate(name, None)

  def tableCreate(name: String, options: TableOptions): TableCreate = tableCreate(name, Some(options))

  def tableCreate(name: String, options: Option[TableOptions] = None): TableCreate = test.tableCreate(name, options.getOrElse(TableOptions()))

  def tableDrop(name: String): TableDrop = test.tableDrop(name)

  def tables: TableList = test.tables

}

trait ExprApi {
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
}

trait RethinkApi extends TableApi with TimeNames with GeometryApi with ExprApi {
  def getInstance = this

  private lazy val _row = new ImplicitVar


  // TODO Fix me, clashes with Sequence and Hash
  def row: ImplicitVar = _row

  def string = row.string

  def branch(predicate: (Var) => Binary, passed: Typed, failed: Typed): Branch = branch(ScalaBooleanPredicate1(predicate), passed, failed)

  def branch(predicate: Binary, passed: Typed, failed: Typed): Branch = Branch(predicate, passed, failed)

  @deprecated("Call .sum on collection")
  def sum(attr: String): BySum = BySum(attr)

  @deprecated("Call .avg on collection")
  def avg(attr: String): ByAvg = ByAvg(attr)

  def random: Random.RandomType[Double] = Random[Double](Seq.empty)

  def random[@specialized(Int, Double, Long) T](values: T*): Random.RandomType[T] = Random[T](values)


  val count = ByCount

  def asc(attr: String): Asc = Asc(attr.wrap)

  def asc(attr: Typed): Asc = Asc(attr.wrap)

  def desc(attr: String): Desc = Desc(attr.wrap)

  def desc(attr: Typed): Desc = Desc(attr.wrap)

  private[this] def compute[T](v: T*)(a: (T, T) => T) = v.drop(1).foldLeft(v.head)(a)


  def call(f: Predicate, values: Typed*): FuncCall = FuncCall(f, values)

  def error(msg: String): UserError = UserError(msg)

  def js(code: String, timeout: Option[Int] = None): JavaScript = JavaScript(code, timeout)

  def json(str: String): Json = Json(str)

  def sub(v: Numeric*): Numeric = compute[Numeric](v: _*)(_ - _)

  def uuid: ast.UUID = new ast.UUID()

  def range(start: Int): Range = Range(start)

  def range(start: Int, end: Int): Range = Range(start, end)

  def minval: ast.MinVal = new ast.MinVal()

  def maxval: ast.MaxVal = new ast.MaxVal()

  def ceil(value: Numeric): ast.Ceil = ast.Ceil(value)

  def floor(value: Numeric): ast.Floor = ast.Floor(value)

  def round(value: Numeric): ast.Round = ast.Round(value)


}


trait TimeNames {

  import com.rethinkscala.ast.TimeName
  import ql2.Ql2.Term.TermType

  private def apply(tt: TermType): TimeName = TimeName(tt)

  val monday = this (TermType.MONDAY)
  val tuesday = this (TermType.TUESDAY)
  val wednesday = this (TermType.WEDNESDAY)
  val thursday = this (TermType.THURSDAY)
  val friday = this (TermType.FRIDAY)
  val saturday = this (TermType.SATURDAY)
  val sunday = this (TermType.SUNDAY)
  val weekdays = Seq(monday, tuesday, wednesday, thursday, friday, saturday, sunday)

  val january = this (TermType.JANUARY)
  val february = this (TermType.FEBRUARY)
  val march = this (TermType.MARCH)
  val april = this (TermType.APRIL)
  val may = this (TermType.MAY)
  val june = this (TermType.JUNE)
  val july = this (TermType.JULY)
  val august = this (TermType.AUGUST)
  val september = this (TermType.SEPTEMBER)

  val october = this (TermType.OCTOBER)
  val november = this (TermType.NOVEMBER)
  val december = this (TermType.DECEMBER)

  val months = Seq(january, february, march, april, june, july, august, september, october, november, december)

  def weekday(dt: DateTime): TimeName = weekdays(dt.dayOfWeek().get() - 1)

  def month(dt: DateTime): TimeName = months(dt.monthOfYear().get())

  def now: Now = new Now()
}
