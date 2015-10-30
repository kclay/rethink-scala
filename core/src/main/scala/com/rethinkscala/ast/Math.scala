package com.rethinkscala.ast

import ql2.Ql2.Term.TermType


case class Eq(left: Typed, right: Typed) extends BiCompareOperQuery with ProduceBinary {
  def termType: TermType = TermType.EQ

  override val stmt = "=="
}

case class Ne(left: Typed, right: Typed) extends BiCompareOperQuery with ProduceBinary {
  def termType: TermType = TermType.NE

  override val stmt = "!="
}

case class Lt(left: Typed, right: Typed) extends BiCompareOperQuery with ProduceBinary {
  def termType: TermType = TermType.LT

  override val stmt = "<"
}

case class Le(left: Typed, right: Typed) extends BiCompareOperQuery with ProduceBinary {
  def termType: TermType = TermType.LE

  override val stmt = "<="
}

case class Gt(left: Typed, right: Typed) extends BiCompareOperQuery with ProduceBinary {
  def termType: TermType = TermType.GT

  override val stmt = ">"
}

case class Ge(left: Typed, right: Typed) extends BiCompareOperQuery with ProduceBinary {
  def termType: TermType = TermType.GE

  override val stmt = ">="
}

case class Not(prev: Typed) extends ProduceBinary {
  override lazy val args = buildArgs(prev)

  def termType: TermType = TermType.NOT

}

case class Sub(left: Numeric, right: Numeric) extends BiCompareOperQuery with ProduceNumeric {
  def termType: TermType = TermType.SUB
}

case class Mul(left: Multiply, right: Numeric) extends BiCompareOperQuery with ProduceNumeric {
  def termType: TermType = TermType.MUL
}

case class Div(left: Numeric, right: Numeric) extends BiCompareOperQuery with ProduceNumeric {
  def termType: TermType = TermType.DIV
}

case class Mod(left: Numeric, right: Numeric) extends BiCompareOperQuery with ProduceNumeric {
  def termType: TermType = TermType.MOD
}

abstract case class And(left: Typed, right: Typed) extends BiCompareOperQuery {

  override lazy val args = buildArgs(left, right)

  def termType: TermType = TermType.AND
}

abstract case class Or(left: Typed, right: Typed) extends BiCompareOperQuery {

  override lazy val args = buildArgs(left, right)

  def termType: TermType = TermType.OR
}


object And {
  def apply(left: Binary, right: Binary): ProduceBinary = new And(left, right) with ProduceBinary

  def apply[T, C[_]](left: Typed, right: Sequence[T, C]): ProduceSeq[T, C] = new And(left, right) with ProduceSeq[T, C]

  def apply(left: Typed, right: Strings): ProduceString = new And(left, right) with ProduceString

  def apply(left: Typed, right: Numeric): ProduceNumeric = new And(left, right) with ProduceNumeric
}


object Or {
  def apply(left: Binary, right: Binary): ProduceBinary = new Or(left, right) with ProduceBinary

  def apply[L, C[_], R >: L, CR[_]](left: Sequence[L, C], right: Sequence[R, CR]): ProduceSeq[L, C] = new Or(left, right) with ProduceSeq[L, C]
}

abstract case class Add(left: Typed, right: Typed) extends BiCompareOperQuery {

  override lazy val args = buildArgs(left, right)

  def termType: TermType = TermType.ADD
}


object Add {

  def any(left: Typed, right: Typed): ProduceAny = new Add(left, right) with ProduceAny

  def apply(left: Numeric, right: Numeric): ProduceNumeric = new Add(left, right) with ProduceNumeric

  def apply(left: Strings, right: Strings): ProduceString = new Add(left, right) with ProduceString

  def apply[L, C[_], R >: L, CR[_]](left: Sequence[L, C], right: Sequence[R, CR]): ProduceSeq[L, C] = new Add(left, right) with ProduceSeq[L, C]
}


case class Ceil(value: Numeric) extends ProduceTypedNumeric[Int] {
  override def termType = TermType.CEIL
}

case class Floor(value: Numeric) extends ProduceTypedNumeric[Int] {
  override def termType = TermType.FLOOR
}

case class Round(value: Numeric) extends ProduceTypedNumeric[Int] {
  override def termType = TermType.ROUND
}