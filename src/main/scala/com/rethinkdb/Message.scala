package com.rethinkdb


import com.rethinkdb.ast._

import ql2.Term.TermType
import ql2.Datum.DatumType


trait Composable {
  def compose(args: Seq[RTerm], optargs: Map[String, RTerm]) = {
    ""
  }
}

trait AssocPair {

  type T
  val key: String
  val value: Any
  lazy val token = Expr(value)

  def pair: T


}


trait BiOpTerm extends RTerm with Composable

trait TopLevelTerm extends RTerm with Composable

trait MethodTerm extends RTerm with Composable

trait ExprWrap

trait Produce[T] {
  def produced: T
}

trait ProducesBoolean extends Produce[Boolean]


trait WithInternalTerm {
  def toInternalTerm: ql2.Term
}

trait Message[T] {


  def toMessage: T

}


trait TermMessage extends Message[ql2.Term] with RTerm {


  def toMessage: ql2.Term = toInternalTerm
}

trait DatumMessage extends Message[ql2.Datum] with RTerm {
  def termType = TermType.DATUM

  def datumType: DatumType.EnumVal

  def build(d: ql2.Datum): ql2.Datum


  override def toInternalTerm: ql2.Term = super.toInternalTerm.setDatum(toMessage)

  def toMessage: ql2.Datum = build(ql2.Datum(Some(datumType)))
}

case class TermAssocPair(key: String, value: Any) extends AssocPair {
  type T = ql2.Term.AssocPair

  def pair = ql2.Term.AssocPair(Some(key), Some(token.toInternalTerm))
}

case class DatumAssocPair(key: String, value: Any) extends AssocPair {
  type T = ql2.Datum.AssocPair

  def pair = ql2.Datum.AssocPair(
    Some(key), Some(token.asInstanceOf[Datum].toMessage))


}

trait RTerm extends WithInternalTerm {


  def toInternalTerm = ql2
    .Term(Some(termType))
    .addAllArgs(args.map(_.toMessage.asInstanceOf[ql2.Term]))
    .addAllOptargs(optargs.map(_.pair.asInstanceOf[ql2.Term.AssocPair]))

  lazy val args = Seq.empty[RTerm]

  protected def buildArgs(args: Any*) = for (a <- args) yield Expr(a)

  lazy val optargs = Iterable.empty[AssocPair]


  def optArgsBuilder(key: String, value: Any): AssocPair = TermAssocPair(key, value)

  protected def buildOptArgs(optargs: Map[String, Option[Any]]): Iterable[AssocPair] = optargs.filter(_._2.isDefined) collect {
    case (key: String, value: Option[Any]) => optArgsBuilder(key, value.get)
  }

  def termType: ql2.Term.TermType.EnumVal

  implicit def term2DataNum(t: RTerm): Datum = t.asInstanceOf[Datum]

  def ==(other: Datum) = Eq(this, other)

  def !=(other: Datum) = Ne(this, other)

  def <(other: Datum) = Lt(this, other)

  def <=(other: Datum) = Le(this, other)

  def >(other: Datum) = Gt(this, other)

  def >=(other: Datum) = Ge(this, other)

  def ~(other: Datum) = Not(this)

  def +(other: Datum) = Add(this, other)

  def >+(other: Datum) = Add(other, this)

  def -(other: Datum) = Sub(this, other)

  def >-(other: Datum) = Sub(other, this)

  def *(other: Datum) = Mul(this, other)

  def >*(other: Datum) = Mul(other, this)

  def /(other: Datum) = Div(this, other)

  def >/(other: Datum) = Div(other, this)

  def %(other: Datum) = Mod(this, other)

  def >%(other: Datum) = Mod(other, this)

  def &(other: Datum) = All(this, other)

  def &&(other: Datum) = All(other, this)

  def &>(other: Datum) = this && other

  // or
  def |(other: Datum) = RAny(this, other)

  // right or
  def >|(other: Datum) = RAny(other, this)

  def contains(attribute: String*) = Contains(this, attribute)

  // setType(termType).addAllArgs(args.map(_.toTerm)).addAllOptargs(optargs.map(_.pair))
}

