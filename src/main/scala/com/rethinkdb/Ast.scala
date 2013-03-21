package com.rethinkdb

import ql2.Ql2._
import scala.concurrent.ExecutionContext.Implicits.global
import ql2.Ql2.Term;
import ql2.Ql2.Query.AssocPair
import com.sun.org.apache.xpath.internal.operations.Lt
import scala._
import java.awt.Container

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 3/19/13
 * Time: 12:06 PM 
 */


object Ast {

  trait Composable {
    def compose(args: Seq[TermBase], optargs: Map[String, TermBase]) = {
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

    def apply(term: TermBase) = term

    def apply(a: Any): TermBase = a match {
      case t: TermBase => t
      case s: Seq[Any] => MakeArray(s)
      //case m: Map[String, Any] => MakeObj(m)
      // case f: PartialFunction =>
      case a: Any => DataNum(a)
    }
  }

  abstract class TermBase(args: Seq[Any] = Seq.empty[Any], optargs: Map[String, Any] = Map.empty[String, Any]) {
    private var _args: Seq[TermBase] = for (a <- args) yield Expr(a)
    private var _optargs: Map[String, TermBase] = optargs.filter(_._2 != None) map {
      case (key: String, value: Any) => (key, Expr(value))
    }
    def termType: Term.TermType

    def run() = {

      None
    }

    implicit def termBaseToTerm(base: TermBase): Term = {

      base.compile(Term.newBuilder().build())


    }

    implicit def optargToAssocPair(i: (String, DataNum)): AssocPair = {

      AssocPair.newBuilder().setKey(i._1).setVal(i._2).build()
    }

    def compile(term: Term): Term = {
      val builder = term.toBuilder.setType(termType)
      for (a <- _args) builder.addArgs(a)
      //for (o <- optargs) builder.addOptargs(o)
      builder.build()


    }

    def ==(other: DataNum) = Eq(this, other)

    def !=(other: DataNum) = Ne(this, other)

    def <(other: DataNum) = Lt(this, other)

    def <=(other: DataNum) = Le(this, other)

    def >(other: DataNum) = Gt(this, other)

    def >=(other: DataNum) = Ge(this, other)

    def ~(other: DataNum) = Not(this)

    def +(other: DataNum) = Add(this, other)

    def >+(other: DataNum) = Add(other, this)

    def -(other: DataNum) = Sub(this, other)

    def >-(other: DataNum) = Sub(other, this)

    def *(other: DataNum) = Mul(this, other)

    def >*(other: DataNum) = Mul(other, this)

    def /(other: DataNum) = Div(this, other)

    def >/(other: DataNum) = Div(other, this)

    def %(other: DataNum) = Mod(this, other)

    def >%(other: DataNum) = Mod(other, this)

    def &(other: DataNum) = All(this, other)

    def &&(other: DataNum) = All(other, this)

    def &>(other: DataNum) = this && other

    // or
    def |(other: DataNum) = RAny(this, other)

    // right or
    def >|(other: DataNum) = RAny(other, this)

    def contains(attr: Seq[Any]) = Contains(this, attr)

  }


  sealed trait BiOpTerm extends Composable

  sealed trait TopLevelTerm extends Composable

  sealed trait MethodTerm extends Composable

  sealed trait ExprWrap;


  case class DataNum(data: Any) extends TermBase with ExprWrap with Composable {


    override def compile(term: Term): Term = {
      val builder = term.toBuilder

      val datumBuilder = builder.getDatumBuilder

      data match {
        case None =>
        case b: Boolean => datumBuilder.setType(Datum.DatumType.R_BOOL).setRBool(b)
        case i@(Int | Float | Long) => datumBuilder.setType(Datum.DatumType.R_NUM).setRNum(i match {
          case a: Int => a.toDouble
          case b: Float => b.toDouble

          case c: Long => c.toDouble
        })
        case s: String => datumBuilder.setType(Datum.DatumType.R_STR).setRStr(s)
        case _ => throw new RuntimeException("Cannot build a query")

      }

      builder.build()
    }

    def termType = Term.TermType.DATUM

    override def compose(args: Seq[TermBase], optargs: Map[String, TermBase]) = data.toString
  }


  case class MakeArray(args: Seq[Any]) extends TermBase(args) with Composable {
    def termType = Term.TermType.MAKE_ARRAY


    // do
    // def ?(func: PartialFunction): TermBase = ???
    override def compose(args: Seq[TermBase], optargs: Map[String, TermBase]) = super.compose(args, optargs)
  }

  case class MakeObj(data: Map[String, Any]) extends TermBase(Seq.empty[Any], data) {
    def termType = Term.TermType.MAKE_OBJ
  }


  case class Var(name: String) extends TermBase(Seq(name)) {
    def termType = Term.TermType.VAR
  }

  case class JavaScript(code: String) extends TermBase(Seq(code)) {
    def termType = Term.TermType.JAVASCRIPT
  }

  case class UserError(error: String) extends TermBase(Seq(error)) {
    def termType = Term.TermType.ERROR
  }

  class ImplicitVar extends TermBase with Composable {
    def termType = Term.TermType.IMPLICIT_VAR
  }


  abstract class BiOperationTerm(left: TermBase, right: TermBase) extends TermBase(Seq(left, right)) with BiOpTerm

  case class Eq(left: TermBase, right: TermBase) extends BiOperationTerm(left, right) {
    def termType = Term.TermType.EQ
  }

  case class Ne(left: TermBase, right: TermBase) extends BiOperationTerm(left, right) {
    def termType = Term.TermType.NE
  }

  case class Lt(left: TermBase, right: TermBase) extends BiOperationTerm(left, right) {
    def termType = Term.TermType.LT
  }

  case class Le(left: TermBase, right: TermBase) extends BiOperationTerm(left, right) {
    def termType = Term.TermType.LE
  }

  case class Gt(left: TermBase, right: TermBase) extends BiOperationTerm(left, right) {
    def termType = Term.TermType.GT
  }


  case class Ge(left: TermBase, right: TermBase) extends BiOperationTerm(left, right) {
    def termType = Term.TermType.GE
  }

  case class Not(prev: TermBase) extends TermBase(Seq(prev)) with Composable {
    def termType = Term.TermType.NOT
  }

  case class Add(left: TermBase, right: TermBase) extends BiOperationTerm(left, right) {
    def termType = Term.TermType.ADD
  }

  case class Sub(left: TermBase, right: TermBase) extends BiOperationTerm(left, right) {
    def termType = Term.TermType.SUB
  }

  case class Mul(left: TermBase, right: TermBase) extends BiOperationTerm(left, right) {
    def termType = Term.TermType.MUL
  }

  case class Div(left: TermBase, right: TermBase) extends BiOperationTerm(left, right) {
    def termType = Term.TermType.DIV
  }

  case class Mod(left: TermBase, right: TermBase) extends BiOperationTerm(left, right) {
    def termType = Term.TermType.MOD
  }

  case class Append(array: Array[Any], other: DataNum) extends TermBase(Seq(array, other)) {
    def termType = Term.TermType.APPEND
  }

  case class Slice(target: TermBase, left: Int, right: Int) extends TermBase(Seq(target, left, right)) {
    def termType = Term.TermType.SLICE
  }

  case class Skip(target: TermBase, amount: Int) extends TermBase(Seq(target, amount)) with MethodTerm {
    def termType = Term.TermType.SKIP
  }


  case class Contains(target: TermBase, attributes: Seq[Any]) extends TermBase(Seq(target, attributes)) {
    def termType = Term.TermType.CONTAINS
  }


  case class All(left: TermBase, right: TermBase) extends BiOperationTerm(left, right) {
    def termType = Term.TermType.ALL
  }

  case class RAny(left: TermBase, right: TermBase) extends BiOperationTerm(left, right) {
    def termType = Term.TermType.ANY
  }


  case class DB(name: String) extends TermBase(Seq(name)) {
    def termType = Term.TermType.DB

    def table_create(name: String, primaryKey: String , dataCenter: String , cacheSize: Int ) {
      TableCreate(name, primaryKey, dataCenter, cacheSize)
    }

    def ^^(name: String, primaryKey: String, dataCenter: String , cacheSize: Int) = this table_create(name, primaryKey, dataCenter, cacheSize)


    def table_drop(name: String) = TableDrop(name)

    def ^-(name: String) = this table_drop (name)

    def table(name: String, useOutDated: Boolean = false) = Table(name, useOutDated)

    def ^(name: String, useOutDated: Boolean = false) = this table(name, useOutDated)


  }


  case class Insert(records: Seq[Map[String, Any]], upsert: Boolean=false) extends TermBase(records, Map("upsert" -> upsert)) {
    def termType = Term.TermType.INSERT
  }

  case class Get(key: String) extends TermBase(Seq(key)) {
    def termType = Term.TermType.GET
  }

  case class TableCreate(name: String, primaryKey: String, dataCenter: String, cacheSize: Int)
    extends TermBase(
      Seq.empty[Any],
      Map("name" -> name, "primary_key" -> primaryKey, "datacenter" -> dataCenter, "cache_size" -> cacheSize)) {
    def termType = Term.TermType.TABLE_CREATE
  }

  case class TableDrop(name: String) extends TermBase(Seq(name)) with MethodTerm {
    def termType = Term.TermType.TABLE_DROP
  }

  case class TableList(db: DB) extends TermBase(Seq(db)) with MethodTerm {
    def termType = Term.TermType.TABLE_LIST
  }

  case class Table(name: String, useOutDated: Boolean = false) extends TermBase(Seq(name), Map("use_outdated" -> useOutDated)) with MethodTerm {
    def termType = Term.TermType.TABLE

    def insert(records: Seq[Map[String, Any]], upsert: Boolean = false) = Insert(records, upsert)

    def ++(records: Seq[Map[String, Any]], upsert: Boolean = false) = this insert(records, upsert)

    def <<(key: String) = Get(key)
  }


}
















