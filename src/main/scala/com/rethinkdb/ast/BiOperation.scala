package com.rethinkdb.ast

import com.rethinkdb.{Term, Composable}
import ql2.Term.TermType

abstract class BiOperationTerm(left: Any, right: Any) extends ProduceBinary with ProduceComparable {
  override lazy val args = buildArgs(left, right)
}

case class Eq(left: Comparable, right: Comparable) extends BiOperationTerm(left, right) {
  def termType = TermType.EQ
}

case class Ne(left: Comparable, right: Comparable) extends BiOperationTerm(left, right) {
  def termType = TermType.NE
}

case class Lt(left: Comparable, right: Comparable) extends BiOperationTerm(left, right) {
  def termType = TermType.LT
}

case class Le(left: Comparable, right: Comparable) extends BiOperationTerm(left, right) {
  def termType = TermType.LE
}

case class Gt(left: Comparable, right: Comparable) extends BiOperationTerm(left, right) {
  def termType = TermType.GT
}


case class Ge(left: Comparable, right: Comparable) extends BiOperationTerm(left, right) {
  def termType = TermType.GE
}

case class Not(prev: Term) extends Term with Composable {
  override lazy val args = buildArgs(prev)

  def termType = TermType.NOT


}

case class Sub(left: ProduceNumeric, right: ProduceNumeric) extends BiOperationTerm(left, right) {
  def termType = TermType.SUB
}

case class Mul(left: ProduceNumeric, right: ProduceNumeric) extends BiOperationTerm(left, right) {
  def termType = TermType.MUL
}

case class Div(left: ProduceNumeric, right: ProduceNumeric) extends BiOperationTerm(left, right) {
  def termType = TermType.DIV
}

case class Mod(left: ProduceNumeric, right: ProduceNumeric) extends BiOperationTerm(left, right) {
  def termType = TermType.MOD
}

case class All(left: ProduceBinary, right: ProduceBinary) extends BiOperationTerm(left, right) {
  def termType = TermType.ALL
}


case class RAny(left: ProduceBinary, right: ProduceBinary) extends BiOperationTerm(left, right) {
  def termType = TermType.ANY
}

case class Add(left: Addable, right: Addable) extends BiOperationTerm(left, right) {
  def termType = TermType.ADD
}


