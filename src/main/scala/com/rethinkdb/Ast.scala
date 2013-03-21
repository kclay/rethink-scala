package com.rethinkdb

import ql2.{Ql2 => p, Ql2}
import scala.concurrent.ExecutionContext.Implicits.global

import com.sun.org.apache.xpath.internal.operations.Lt
import scala._
import utils.Printable


/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 3/19/13
 * Time: 12:06 PM 
 */


object Ast {


  trait Composable {
    def compose(args: Seq[Term], optargs: Map[String, Term]) = {
      ""
    }
  }


  /*case class T(values: Any*) extends Iterator[String] {
    def hasNext = ???

    var pos = 0

    def next() = {
      for (itr <- values(0)) {
        for (sub <- itr) yield sub
        for (token <- itr)
      }
    }
  }*/

  object Expr {

    def apply(term: Term) = term

    def apply(a: Any): Term = a match {
      case t: Term => t
      case s: Seq[Any] => MakeArray(s)
      //case m: Map[String, Any] => MakeObj(m)
      // case f: PartialFunction =>
      //case a: Any => DatNum(a)
    }
  }

  trait Term {
    lazy val args = Seq.empty[Term]

    protected def buildArgs(args: Any*) = for (a <- args) yield Expr(a)

    lazy val optargs = Map.empty[String, Term]


    protected def buildOptArgs(optargs: Map[String, Any]) = optargs.filter(_._2 != None) map {
      case (key: String, value: Any) => (key, Expr(value))
    }

    def termType: p.Term.TermType

    def run() = {

      None
    }

    implicit def termBaseToTerm(base: Term): p.Term = {

      base.compile(p.Term.newBuilder().build())


    }

    implicit def optargToAssocPair(i: (String, Term)): p.Term.AssocPair = {

      p.Term.AssocPair.newBuilder().setKey(i._1).setVal(i._2).build()
    }

    def compile(term: p.Term): p.Term = {
      val builder = term.toBuilder.setType(termType)
      // for (a <- args) builder.addArgs(a)
      //for (o <- optargs) builder.addOptargs(o)
      builder.build()


    }

    def ==(other: Term) = Eq(this, other)

    def !=(other: Term) = Ne(this, other)

    def <(other: Term) = Lt(this, other)

    def <=(other: Term) = Le(this, other)

    def >(other: Term) = Gt(this, other)

    def >=(other: Term) = Ge(this, other)

    def ~(other: Term) = Not(this)

    def +(other: Term) = Add(this, other)

    def >+(other: Term) = Add(other, this)

    def -(other: Term) = Sub(this, other)

    def >-(other: Term) = Sub(other, this)

    def *(other: Term) = Mul(this, other)

    def >*(other: Term) = Mul(other, this)

    def /(other: Term) = Div(this, other)

    def >/(other: Term) = Div(other, this)

    def %(other: Term) = Mod(this, other)

    def >%(other: Term) = Mod(other, this)

    def &(other: Term) = All(this, other)

    def &&(other: Term) = All(other, this)

    def &>(other: Term) = this && other

    // or
    def |(other: Term) = RAny(this, other)

    // right or
    def >|(other: Term) = RAny(other, this)

    def contains(attr: Seq[Any]) = Contains(this, attr)

  }


  sealed trait BiOpTerm extends Composable

  sealed trait TopLevelTerm extends Composable

  sealed trait MethodTerm extends Composable

  sealed trait ExprWrap

  sealed trait Datum extends Term with ExprWrap with Composable {

    override def compile(term: p.Term): p.Term = {
      val builder = term.toBuilder
      build(builder.getDatumBuilder)

      builder.build()
    }

    def termType = p.Term.TermType.DATUM

    def build(builder: p.Datum.Builder): p.Datum.Builder
  }

  case class StringDatum(value: String) extends Datum {
    protected def build(builder: p.Datum.Builder): p.Datum.Builder = {
      builder.setType(p.Datum.DatumType.R_STR).setRStr(value)
    }
  }

  class NoneDatum extends Datum {


    def build(builder: p.Datum.Builder): p.Datum.Builder = {
      builder
    }
  }

  case class BooleanDatum(value: Boolean) extends Datum {


    def build(builder: p.Datum.Builder) {
      builder.setType(p.Datum.DatumType.R_BOOL).setRBool(value)
    }
  }

  case class NumberDatum(value: Double) extends Datum {
    def build(builder: p.Datum.Builder) = {

      builder.setType(p.Datum.DatumType.R_NUM).setRNum(value)
    }
  }

  case class StringDatum(value: String) extends Datum {
    def build(builder: p.Datum.Builder) = {

      builder.setType(p.Datum.DatumType.R_STR).setRStr(value)
    }
  }


  case class MakeArray(array: Array[Any]) extends Term with Composable {
    lazy val args = buildArgs(for (a <- array) yield a)

    def termType = p.Term.TermType.MAKE_ARRAY


    // do
    // def ?(func: PartialFunction): Term = ???
    override def compose(args: Seq[Term], optargs: Map[String, Term]) = super.compose(args, optargs)
  }

  case class MakeObj(data: Map[String, Any]) extends Term {
    lazy val optargs = buildOptArgs(data)

    def termType = p.Term.TermType.MAKE_OBJ
  }


  case class Var(name: String) extends Term {
    lazy val args = buildArgs(name)

    def termType = p.Term.TermType.VAR
  }

  case class JavaScript(code: String) extends Term(Seq(code)) {
    def termType = p.Term.TermType.JAVASCRIPT
  }

  case class UserError(error: String) extends Term(Seq(error)) {
    def termType = p.Term.TermType.ERROR
  }

  class ImplicitVar extends Term with Composable {
    def termType = p.Term.TermType.IMPLICIT_VAR
  }


  abstract class BiOperationTerm(left: Term, right: Term) extends Term with BiOpTerm {
    lazy val args = buildArgs(left, right)
  }

  case class Eq(left: Term, right: Term) extends BiOperationTerm(left, right) {
    def termType = p.Term.TermType.EQ
  }

  case class Ne(left: Term, right: Term) extends BiOperationTerm(left, right) {
    def termType = p.Term.TermType.NE
  }

  case class Lt(left: Term, right: Term) extends BiOperationTerm(left, right) {
    def termType = p.Term.TermType.LT
  }

  case class Le(left: Term, right: Term) extends BiOperationTerm(left, right) {
    def termType = p.Term.TermType.LE
  }

  case class Gt(left: Term, right: Term) extends BiOperationTerm(left, right) {
    def termType = p.Term.TermType.GT
  }


  case class Ge(left: Term, right: Term) extends BiOperationTerm(left, right) {
    def termType = p.Term.TermType.GE
  }

  case class Not(prev: Term) extends Term with Composable {
    lazy val args = buildArgs(prev)

    def termType = p.Term.TermType.NOT
  }

  case class Add(left: Term, right: Term) extends BiOperationTerm(left, right) {
    def termType = p.Term.TermType.ADD
  }

  case class Sub(left: Term, right: Term) extends BiOperationTerm(left, right) {
    def termType = p.Term.TermType.SUB
  }

  case class Mul(left: Term, right: Term) extends BiOperationTerm(left, right) {
    def termType = p.Term.TermType.MUL
  }

  case class Div(left: Term, right: Term) extends BiOperationTerm(left, right) {
    def termType = p.Term.TermType.DIV
  }

  case class Mod(left: Term, right: Term) extends BiOperationTerm(left, right) {
    def termType = p.Term.TermType.MOD
  }

  case class Append(array: Array[Any], other: Term) extends Term {
    lazy val args = buildArgs(array, other)

    def termType = p.Term.TermType.APPEND
  }

  case class Slice(target: Term, left: Int, right: Int) extends Term {
    lazy val args = buildArgs(target, left, right)

    def termType = p.Term.TermType.SLICE
  }

  case class Skip(target: Term, amount: Int) extends Term with MethodTerm {
    lazy val args = buildArgs(target, amount)

    def termType = p.Term.TermType.SKIP
  }


  case class Contains(target: Term, attributes: Seq[Any]) extends Term {
    lazy val args = buildArgs(target, attributes)

    def termType = p.Term.TermType.CONTAINS
  }


  case class All(left: Term, right: Term) extends BiOperationTerm(left, right) {
    def termType = p.Term.TermType.ALL
  }

  case class RAny(left: Term, right: Term) extends BiOperationTerm(left, right) {
    def termType = p.Term.TermType.ANY
  }


  case class DB(name: String) extends Term {
    lazy val args = buildArgs(name)

    def termType = p.Term.TermType.DB

    def table_create(name: String, primaryKey: Option[String], dataCenter: Option[String], cacheSize: Option[Int]) {
      TableCreate(name, primaryKey, dataCenter, cacheSize)
    }

    def ^^(name: String, primaryKey: Option[String], dataCenter: Option[String], cacheSize: Option[Int]) = this table_create(name, primaryKey, dataCenter, cacheSize)


    def table_drop(name: String) = TableDrop(name)

    def ^-(name: String) = this table_drop (name)

    def table(name: String, useOutDated: Boolean = false) = Table(name, useOutDated)

    def ^(name: String, useOutDated: Boolean = false) = this table(name, useOutDated)


  }


  case class Insert(records: Seq[Map[String, Any]], upsert: Option[Boolean] = None) extends Term {
    lazy val args = buildArgs(records)
    lazy val optargs = buildOptArgs(Map("upsert" -> upsert))

    def termType = p.Term.TermType.INSERT
  }

  case class Get(key: String) extends Term {
    lazy val args = buildArgs(key)

    def termType = p.Term.TermType.GET
  }

  case class TableCreate(name: String, primaryKey: Option[String] = None, dataCenter: Option[String] = None, cacheSize: Option[Int] = None) extends Term {

    lazy val args = buildArgs(name)
    lazy val optargs = buildOptArgs(Map("name" -> name, "primary_key" -> primaryKey, "datacenter" -> dataCenter, "cache_size" -> cacheSize))

    def termType = p.Term.TermType.TABLE_CREATE
  }

  case class TableDrop(name: String) extends Term with MethodTerm {
    lazy val args = buildArgs(name)

    def termType = p.Term.TermType.TABLE_DROP
  }

  case class TableList(db: DB) extends Term(Seq(db)) with MethodTerm {
    def termType = p.Term.TermType.TABLE_LIST
  }

  case class Table(name: String, useOutDated: Option[Boolean] = None) extends Term with MethodTerm {
    lazy val args = buildArgs(name)
    lazy val optargs = buildOptArgs(Map("use_outdated" -> useOutDated))

    def termType = p.Term.TermType.TABLE

    def insert(records: Seq[Map[String, Any]], upsert: Boolean = false) = Insert(records, upsert)

    def ++(records: Seq[Map[String, Any]], upsert: Boolean = false) = this insert(records, upsert)

    def <<(key: String) = Get(key)
  }


}
















