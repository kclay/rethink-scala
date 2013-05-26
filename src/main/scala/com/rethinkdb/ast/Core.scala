package com.rethinkdb.ast

import com.rethinkdb.{Composable, Term}

import ql2.Term.TermType


case class MakeArray(array: Seq[Any]) extends Term with Composable {
  override lazy val args = buildArgs(array: _*)

  // def datumTermType:TermType.EnumVal=ql2.Datum
  def termType = TermType.MAKE_ARRAY


  // do
  // def ?(func: PartialFunction): Term = ???
  override def compose(args: Seq[Term], optargs: Map[String, Term]) = super.compose(args, optargs)
}

case class MakeObj(data: Map[String, Option[Any]]) extends Term {
  override lazy val optargs = buildOptArgs(data)

  def termType = TermType.MAKE_OBJ
}


case class Var(name: String) extends Term {
  override lazy val args = buildArgs(name)

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