package com.rethinkdb

import ql2.Ql2._
import scala.concurrent.ExecutionContext.Implicits.global
import ql2.Ql2.Term;
import ql2.Ql2.Query.AssocPair
import com.sun.org.apache.xpath.internal.operations.Lt

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 3/19/13
 * Time: 12:06 PM 
 */


object Expr {

  def apply(term: TermBase) = term

  def apply(a: Any): TermBase = a match {
    case t: TermBase => t
    case s: Seq[Any] =>
    case m: Map => MakeObj(m)
    case f: PartialFunction =>
    case a: Any => DataNum(a)
  }
}

abstract class TermBase(args: Seq[Any] = Seq.empty[Any], optargs: Map[String, Any] = Map.empty[String, Any]) {
  private var _args: Seq[TermBase] = for (a <- args) yield Expr(a)
  private var _optargs: Map[String, Datum] = Map.empty[String, Datum]
  val termType: Term.TermType

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

  def |(other: DataNum) = RAny(this, other)

  def >|(other: DataNum) = RAny(other, this)
}

trait BaseQuery {


  val term: Term.TermType


}


sealed trait ExprWrap;


abstract class DataNum(data: Any) extends TermBase with ExprWrap {


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
}


case class MakeArray(args: Seq[Any]) extends TermBase(args) {
  val termType = Term.TermType.MAKE_ARRAY

  // do
  def ?(func: PartialFunction): TermBase = ???
}

case class MakeObj(data: Map[String, Any]) extends TermBase(Seq.empty[Seq], data) {
  val termType = Term.TermType.MAKE_OBJ
}


case class Var(name: String) {
  val term = Term.TermType.VAR
}

class JavaScript extends BaseQuery {
  val term = Term.TermType.JAVASCRIPT
}

sealed trait BiOpTerm

abstract class BiOperationTerm(left: TermBase, right: TermBase) extends TermBase(Seq(left, right)) with BiOpTerm

case class Eq(left: TermBase, right: TermBase) extends BiOperationTerm(left, right) {
  val termType = Term.TermType.EQ
}

case class Ne(left: TermBase, right: TermBase) extends BiOperationTerm(left, right) {
  val termType = Term.TermType.NE
}

case class Lt(left: TermBase, right: TermBase) extends BiOperationTerm(left, right) {
  val termType = Term.TermType.LT
}

case class Le(left: TermBase, right: TermBase) extends BiOperationTerm(left, right) {
  val termType = Term.TermType.LE
}

case class Gt(left: TermBase, right: TermBase) extends BiOperationTerm(left, right) {
  val termType = Term.TermType.GT
}

case class Le(left: TermBase, right: TermBase) extends BiOperationTerm(left, right) {
  val termType = Term.TermType.LE
}

case class Gt(left: TermBase, right: TermBase) extends BiOperationTerm(left, right) {
  val termType = Term.TermType.GT
}

case class Ge(left: TermBase, right: TermBase) extends BiOperationTerm(left, right) {
  val termType = Term.TermType.GE
}

case class Not(prev: TermBase) extends TermBase(Seq(prev)) {
  val termType = Term.TermType.NOT
}

case class Add(left: TermBase, right: TermBase) extends BiOperationTerm(left, right) {
  val termType = Term.TermType.ADD
}

case class Sub(left: TermBase, right: TermBase) extends BiOperationTerm(left, right) {
  val termType = Term.TermType.SUB
}

case class Mul(left: TermBase, right: TermBase) extends BiOperationTerm(left, right) {
  val termType = Term.TermType.MUL
}

case class Div(left: TermBase, right: TermBase) extends BiOperationTerm(left, right) {
  val termType = Term.TermType.DIV
}

case class Mod(left: TermBase, right: TermBase) extends BiOperationTerm(left, right) {
  val termType = Term.TermType.MOD
}

case class All(left: TermBase, right: TermBase) extends BiOperationTerm(left, right) {
  val termType = Term.TermType.ALL
}

case class RAny(left: TermBase, right: TermBase) extends BiOperationTerm(left, right) {
  val termType = Term.TermType.ANY
}

class Append(array: Array, other: DataNum) extends TermBase(Seq(array, other)) {
  val termType = Term.TermType.APPEND
}

class Slice(sequence: Seq[Any], start: Int, end: Int) extends TermBase(Seq(sequence, start, end)) {
  val termType = Term.TermType.SLICE
}

class Sub(left: TermBase, right: TermBase) extends BiOperationTerm(left, right) {
  val termType = Term.TermType.SUB
}

















