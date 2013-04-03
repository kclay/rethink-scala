package com.rethinkdb.ast

import com.rethinkdb.{Composable, Term}
import ql2.{Ql2=>p}

import com.rethinkdb.conversions.Tokens._


case class MakeArray(array: Seq[Any]) extends Term with Composable {
  override lazy val args = buildArgs(array: _*)

  // def datumTermType:TokenType=p.Datum
  def termType: TokenType = p.Term.TermType.MAKE_ARRAY


  // do
  // def ?(func: PartialFunction): Term = ???
  override def compose(args: Seq[Term], optargs: Map[String, Term]) = super.compose(args, optargs)
}

case class MakeObj(data: Map[String, Option[Any]]) extends Term {
  override lazy val optargs = buildOptArgs(data)

  def termType: TokenType = p.Term.TermType.MAKE_OBJ
}


case class Var(name: String) extends Term {
  override lazy val args = buildArgs(name)

  def termType: TokenType = p.Term.TermType.VAR
}

case class JavaScript(code: String) extends Term {
  override lazy val args = buildArgs(code)

  def termType: TokenType = p.Term.TermType.JAVASCRIPT
}

case class UserError(error: String) extends Term {
  override lazy val args = buildArgs(error)

  def termType: TokenType = p.Term.TermType.ERROR
}

class ImplicitVar extends Term with Composable {
  def termType: TokenType = p.Term.TermType.IMPLICIT_VAR
}