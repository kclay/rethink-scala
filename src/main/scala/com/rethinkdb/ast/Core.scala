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


case class Var(id: Int) extends Term with ProduceAny with  WithAny{
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

class ImplicitVar extends Term with ProduceAny with WithAny {

  def termType = TermType.IMPLICIT_VAR
}


object Core {
  val row = new ImplicitVar()
}







