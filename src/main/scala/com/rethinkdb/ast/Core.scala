package com.rethinkdb.ast

import com.rethinkdb.{ InfoResult, DocumentConversion, Term }

import ql2.Term.TermType

case class MakeArray(array: Seq[Any]) extends Term with ArrayTyped {
  override lazy val args = buildArgs(array: _*)

  // def datumTermType:TermType.EnumVal=ql2.Datum
  def termType = TermType.MAKE_ARRAY

}

case class MakeObj(data: Map[String, Any]) extends Term with MapTyped {
  override lazy val optargs = buildOptArgs2(data)

  def termType = TermType.MAKE_OBJ
}

case class Var(id: Int) extends Term with ProduceAny {
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

class ImplicitVar extends Term with ProduceAny {

  def termType = TermType.IMPLICIT_VAR
}

case class Info(target: Typed) extends ProduceDocument with DocumentConversion[InfoResult] {
  def termType = TermType.INFO
}

object Core {
  val row = new ImplicitVar()
}

