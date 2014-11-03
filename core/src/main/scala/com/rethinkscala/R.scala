package com.rethinkscala

import com.rethinkscala.ast._
import org.joda.time.{ReadableInstant, DateTime}
import com.rethinkscala.ast.StringDatum

import com.rethinkscala.ast.JavaScript
import com.rethinkscala.ast.TableDrop
import com.rethinkscala.ast.Branch
import com.rethinkscala.ast.DBCreate
import com.rethinkscala.ast.DB
import com.rethinkscala.ast.Asc
import scala.Some
import com.rethinkscala.ast.FuncCall
import com.rethinkscala.ast.DBDrop
import com.rethinkscala.ast.TableList
import com.rethinkscala.ast.Desc
import com.rethinkscala.ast.BySum
import com.rethinkscala.ast.Table
import com.rethinkscala.ast.BooleanDatum
import com.rethinkscala.ast.DBList
import com.rethinkscala.ast.ScalaBooleanPredicate1
import com.rethinkscala.ast.UserError
import com.rethinkscala.ast.ByAvg
import com.rethinkscala.ast.TableCreate
import com.rethinkscala.ast.NumberDatum
import scala.specialized


/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 9/12/13
 * Time: 3:14 PM
 *
 */


trait RethinkApi extends TimeNames {
  def getInstance = this

  private lazy val _row = new ImplicitVar


  // def http

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

  // TODO Fix me, clashes with Sequence and Hash
  def row: ImplicitVar = _row

  def string = row.string


  //def row[T<:Sequence](name: String)(implicit d:DummyImplicit) = _row.asInstanceOf[T] field name


  lazy val test = db("test")

  def table(name: String): Table[Document] = table(name, None)

  def table(name: String, useOutDated: Boolean): Table[Document] = table(name, Some(useOutDated))

  def table(name: String, useOutDated: Option[Boolean] = None): Table[Document] = Table[Document](name, useOutDated)


  def tableAs[T <: Document](name: String): Table[T] = tableAs[T](name, None)

  def tableAs[T <: Document](name: String, useOutDated: Boolean): Table[T] = tableAs[T](name, Some(useOutDated))

  def tableAs[T <: Document](name: String, useOutDated: Option[Boolean] = None): Table[T] = Table[T](name, useOutDated)

  def db(name: String) = DB(name)

  def dbCreate(name: String) = DBCreate(name)

  def dbDrop(name: String) = DBDrop(name)

  def dbs = DBList()


  def tableCreate(name: String): TableCreate = tableCreate(name, None)

  def tableCreate(name: String, options: TableOptions): TableCreate = tableCreate(name, Some(options))

  def tableCreate(name: String, options: Option[TableOptions] = None): TableCreate = test.tableCreate(name, options.getOrElse(TableOptions()))

  def tableDrop(name: String) = test.tableDrop(name)

  def tables = test.tables


  def branch(predicate: (Var) => Binary, passed: Typed, failed: Typed): Branch = branch(ScalaBooleanPredicate1(predicate), passed, failed)

  def branch(predicate: Binary, passed: Typed, failed: Typed): Branch = Branch(predicate, passed, failed)

  def sum(attr: String) = BySum(attr)

  def avg(attr: String) = ByAvg(attr)

  def random = Random[Double](Seq.empty)

  def random[@specialized(Int, Double, Long) T](values: T*) = Random[T](values)

  // def random(values:Float*) = Random[Float,Float](values,true)

  val count = ByCount

  def asc(attr: String) = Asc(attr)

  def desc(attr: String) = Desc(attr)

  private[this] def compute[T](v: T*)(a: (T, T) => T) = v.drop(1).foldLeft(v.head)(a)

  // def eq(base:Comparable,v:Comparable *) = compute[Comparable](v:_*)()
  //def add(v: Addition*) = compute[Addition](v: _*)(_ + _)


  def sub(v: Numeric*) = compute[Numeric](v: _*)(_ - _)

  def call(f: Predicate, values: Typed*) = FuncCall(f, values)

  def error(msg: String) = UserError(msg)

  def js(code: String, timeout: Option[Int] = None) = JavaScript(code, timeout)

  def json(str: String) = Json(str)

  def uuid = new ast.UUID()


}


trait TimeNames {

  import ql2.Ql2.Term.TermType
  import com.rethinkscala.ast.TimeName

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
