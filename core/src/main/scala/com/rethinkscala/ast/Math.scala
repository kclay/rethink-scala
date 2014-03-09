package com.rethinkscala.ast

import ql2.Ql2.Term.TermType


case class Eq(left: Literal, right: Literal) extends BiCompareOperQuery with ProduceBinary {
  def termType = TermType.EQ

  override val stmt = "=="
}

case class Ne(left: Literal, right: Literal) extends BiCompareOperQuery with ProduceBinary {
  def termType = TermType.NE

  override val stmt = "!="
}

case class Lt(left: Literal, right: Literal) extends BiCompareOperQuery with ProduceBinary {
  def termType = TermType.LT

  override val stmt = "<"
}

case class Le(left: Literal, right: Literal) extends BiCompareOperQuery with ProduceBinary {
  def termType = TermType.LE

  override val stmt = "<="
}

case class Gt(left: Literal, right: Literal) extends BiCompareOperQuery with ProduceBinary {
  def termType = TermType.GT

  override val stmt = ">"
}

case class Ge(left: Literal, right: Literal) extends BiCompareOperQuery with ProduceBinary {
  def termType = TermType.GE

  override val stmt = ">="
}

case class Not(prev: Literal) extends Query {
  override lazy val args = buildArgs(prev)

  def termType = TermType.NOT

  /*
  override private[rethinkscala] def print(args: Seq[String], opt: Map[String, String]) = args(0) match{
    case d:Datum=>""
  } */
}

case class Sub(left: Numeric, right: Numeric) extends BiCompareOperQuery with ProduceNumeric {
  def termType = TermType.SUB
}

case class Mul(left: Multiply, right: Numeric) extends BiCompareOperQuery with ProduceNumeric {
  def termType = TermType.MUL
}

case class Div(left: Numeric, right: Numeric) extends BiCompareOperQuery with ProduceNumeric {
  def termType = TermType.DIV
}

case class Mod(left: Numeric, right: Numeric) extends BiCompareOperQuery with ProduceNumeric {
  def termType = TermType.MOD
}

case class All(left: Binary, right: Binary) extends BiCompareOperQuery with ProduceBinary {
  def termType = TermType.ALL
}

case class Or(left: Binary, right: Binary) extends BiCompareOperQuery with ProduceBinary {
  def termType = TermType.ANY
}

trait Add extends BiCompareOperQuery with Literal {


  def termType = TermType.ADD
}

case class AnyAdd(left: Addition, right: Addition) extends Add with ProduceAny


case class NumericAdd(left: Addition, right: Addition) extends Add with ProduceNumeric

case class StringAdd(left: Addition, right: Addition) extends Add with ProduceString