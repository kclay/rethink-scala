package com.rethinkdb


import com.rethinkdb.ast._

import ql2.Term.TermType
import ql2.Datum.DatumType


trait Composable {
  def compose(args: Seq[Term], optargs: Map[String, Term]) = {
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


trait BiOpTerm extends Term with Composable

trait TopLevelTerm extends Term with Composable

trait MethodTerm extends Term with Composable

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


trait TermMessage extends Message[ql2.Term] with Term {


  def toMessage: ql2.Term = toInternalTerm
}

trait DatumMessage extends Message[ql2.Datum] with Term {
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

trait Term extends WithInternalTerm {


  def toInternalTerm = ql2
    .Term(Some(termType))
    .addAllArgs(args.map(_.toInternalTerm))
    .addAllOptargs(optargs.map(_.pair.asInstanceOf[ql2.Term.AssocPair]))

  lazy val args = Seq.empty[Term]

  protected def buildArgs(args: Any*):Seq[Term] = for (a <- args) yield Expr(a)

  lazy val optargs = Iterable.empty[AssocPair]


  def optArgsBuilder(key: String, value: Any): AssocPair = TermAssocPair(key, value)

  protected def buildOptArgs(optargs: Map[String, Option[Any]]): Iterable[AssocPair] = optargs.filter(_._2.isDefined) collect {
    case (key: String, value: Option[Any]) => optArgsBuilder(key, value.get)
  }

  def termType: ql2.Term.TermType.EnumVal

 // implicit def term2DataNum(t: Term): Datum = t.asInstanceOf[Datum]

  def ==(other: Term) = Eq(this, other)

  def !=(other: Term) = Ne(this, other)

  def <(other: Term) = Lt(this, other)

  def <=(other: Term) = Le(this, other)

  def >(other: Term) = Gt(this, other)

  def >=(other: Term) = Ge(this, other)

  def ~(other: Term) = Not(this)

  def +(other:Term)=Add(this,other)

  def +=(other: StringDatum) = Add(this, other)

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

  def contains(attribute: String*) = Contains(this, attribute)

  // setType(termType).addAllArgs(args.map(_.toTerm)).addAllOptargs(optargs.map(_.pair))
}

