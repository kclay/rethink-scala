package com.rethinkdb.ast

import com.rethinkdb.{Term, Composable, BiOpTerm}
import ql2.Term.TermType

abstract class BiOperationTerm(left: Term, right: Term) extends BiOpTerm {
  override lazy val args = buildArgs(left, right)
}

case class Eq(left: Term, right: Term) extends BiOperationTerm(left, right) {
  def termType = TermType.EQ
}

case class Ne(left: Term, right: Term) extends BiOperationTerm(left, right) {
  def termType = TermType.NE
}

case class Lt(left: Term, right: Term) extends BiOperationTerm(left, right) {
  def termType = TermType.LT
}

case class Le(left: Term, right: Term) extends BiOperationTerm(left, right) {
  def termType = TermType.LE
}

case class Gt(left: Term, right: Term) extends BiOperationTerm(left, right) {
  def termType = TermType.GT
}


case class Ge(left: Term, right: Term) extends BiOperationTerm(left, right) {
  def termType = TermType.GE
}

case class Not(prev: Term) extends Term with Composable {
  override lazy val args = buildArgs(prev)

  def termType = TermType.NOT
}

case class Add(left: Term, right: Term) extends BiOperationTerm(left, right) {
  def termType = TermType.ADD
}

case class Sub(left: Term, right: Term) extends BiOperationTerm(left, right) {
  def termType = TermType.SUB
}

case class Mul(left: Term, right: Term) extends BiOperationTerm(left, right) {
  def termType = TermType.MUL
}

case class Div(left: Term, right: Term) extends BiOperationTerm(left, right) {
  def termType = TermType.DIV
}

case class Mod(left: Term, right: Term) extends BiOperationTerm(left, right) {
  def termType = TermType.MOD
}

case class All(left: Term, right: Term) extends BiOperationTerm(left, right) {
  def termType = TermType.ALL
}


case class RAny(left: Term, right: Term) extends BiOperationTerm(left, right) {
  def termType = TermType.ANY
}

