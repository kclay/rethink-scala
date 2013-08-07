package com.rethinkscala.ast

import com.rethinkscala.Term
import ql2.Ql2.Term.TermType

abstract class Math {
  val left: Typed
  val right: Typed
}

case class Eq(left: Literal, right: Literal) extends Math with ProduceBinary {
  def termType = TermType.EQ
}

case class Ne(left: Literal, right: Literal) extends Math with ProduceBinary {
  def termType = TermType.NE
}

case class Lt(left: Literal, right: Literal) extends Math with ProduceBinary {
  def termType = TermType.LT
}

case class Le(left: Literal, right: Literal) extends Math with ProduceBinary {
  def termType = TermType.LE
}

case class Gt(left: Literal, right: Literal) extends Math with ProduceBinary {
  def termType = TermType.GT
}

case class Ge(left: Literal, right: Literal) extends Math with ProduceBinary {
  def termType = TermType.GE
}

case class Not(prev: Literal) extends Term {
  override lazy val args = buildArgs(prev)

  def termType = TermType.NOT

}

case class Sub(left: Numeric, right: Numeric) extends Math with ProduceNumeric {
  def termType = TermType.SUB
}

case class Mul(left: Multiply, right: Numeric) extends Math with ProduceNumeric {
  def termType = TermType.MUL
}

case class Div(left: Numeric, right: Numeric) extends Math with ProduceNumeric {
  def termType = TermType.DIV
}

case class Mod(left: Numeric, right: Numeric) extends Math with ProduceNumeric {
  def termType = TermType.MOD
}

case class All(left: Binary, right: Binary) extends Math with ProduceBinary {
  def termType = TermType.ALL
}

case class Or(left: Binary, right: Binary) extends Math with ProduceBinary {
  def termType = TermType.ANY
}

case class Add(left: Addition, right: Addition) extends Math with ProduceNumeric {
  def termType = TermType.ADD
}

