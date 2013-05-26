package com.rethinkdb.ast

import com.rethinkdb.{Composable, RTerm}

import ql2.Term.TermType


case class MakeArray(array: Seq[Any]) extends RTerm with Composable {
  override lazy val args = buildArgs(array: _*)

  // def datumTermType:TermType.EnumVal=ql2.Datum
  def termType = TermType.MAKE_ARRAY


  // do
  // def ?(func: PartialFunction): RTerm = ???
  override def compose(args: Seq[RTerm], optargs: Map[String, RTerm]) = super.compose(args, optargs)
}

case class MakeObj(data: Map[String, Option[Any]]) extends RTerm {
  override lazy val optargs = buildOptArgs(data)

  def termType = TermType.MAKE_OBJ
}


case class Var(name: String) extends RTerm {
  override lazy val args = buildArgs(name)

  def termType = TermType.VAR
}

case class JavaScript(code: String) extends RTerm {
  override lazy val args = buildArgs(code)

  def termType = TermType.JAVASCRIPT
}

case class UserError(error: String) extends RTerm {
  override lazy val args = buildArgs(error)

  def termType = TermType.ERROR
}

class ImplicitVar extends RTerm with Composable {
  def termType = TermType.IMPLICIT_VAR
}