package com.rethinkdb.ast

import com.rethinkdb.{Term, Composable}

import ql2.Term.TermType


case class MakeArray(array: Seq[Any]) extends Term with Composable {
  override lazy val args = buildArgs(array: _*)

  // def datumTermType:TermType.EnumVal=ql2.Datum
  def termType = TermType.MAKE_ARRAY


  // do
  // def ?(func: PartialFunction): Term = ???
  override def compose(args: Seq[Term], optargs: Map[String, Term]) = super.compose(args, optargs)
}

case class MakeObj(data: Map[String, Any]) extends Term {
  override lazy val optargs = buildOptArgs2(data)


  def termType = TermType.MAKE_OBJ
}


case class Var(id: Int) extends Term {
  override lazy val args = buildArgs(id)

  def termType = TermType.VAR
}

case class JavaScript(code: String) extends Term {
  override lazy val args = buildArgs(code)

  def termType = TermType.JAVASCRIPT
}

case class UserError(error: String) extends Term {
  override lazy val args = buildArgs(error)

  def termType = TermType.ERROR
}

class ImplicitVar extends Term with Composable {
  def termType = TermType.IMPLICIT_VAR
}


object Core {
  val row = new ImplicitVar()
}




trait Operations {
  self: Term =>


  def ==(other: Term) = Eq(this, other)

  def !=(other: Term) = Ne(this, other)

  def <(other: Term) = Lt(this, other)

  def <=(other: Term) = Le(this, other)

  def >(other: Term) = Gt(this, other)

  def >=(other: Term) = Ge(this, other)

  def ~(other: Term) = Not(this)

  def +(other: Term) = Add(this, other)

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

  def attr(name: String) = GetAttr(this, name)

  def \(name: String) = attr(name)

  def row(name: String) = Core.row \ name

  def map(f: Predicate) = RMap(this, f)
}