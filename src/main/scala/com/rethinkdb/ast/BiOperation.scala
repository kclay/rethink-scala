package com.rethinkdb.ast

import com.rethinkdb.{RTerm, Composable, BiOpTerm}
import ql2.Term.TermType

abstract class BiOperationTerm(left: RTerm, right: RTerm) extends BiOpTerm {
  override lazy val args = buildArgs(left, right)
}

case class Eq(left: RTerm, right: RTerm) extends BiOperationTerm(left, right) {
  def termType = TermType.EQ
}

case class Ne(left: RTerm, right: RTerm) extends BiOperationTerm(left, right) {
  def termType = TermType.NE
}

case class Lt(left: RTerm, right: RTerm) extends BiOperationTerm(left, right) {
  def termType = TermType.LT
}

case class Le(left: RTerm, right: RTerm) extends BiOperationTerm(left, right) {
  def termType = TermType.LE
}

case class Gt(left: RTerm, right: RTerm) extends BiOperationTerm(left, right) {
  def termType = TermType.GT
}


case class Ge(left: RTerm, right: RTerm) extends BiOperationTerm(left, right) {
  def termType = TermType.GE
}

case class Not(prev: RTerm) extends RTerm with Composable {
  override lazy val args = buildArgs(prev)

  def termType = TermType.NOT
}

case class Add(left: RTerm, right: RTerm) extends BiOperationTerm(left, right) {
  def termType = TermType.ADD
}

case class Sub(left: RTerm, right: RTerm) extends BiOperationTerm(left, right) {
  def termType = TermType.SUB
}

case class Mul(left: RTerm, right: RTerm) extends BiOperationTerm(left, right) {
  def termType = TermType.MUL
}

case class Div(left: RTerm, right: RTerm) extends BiOperationTerm(left, right) {
  def termType = TermType.DIV
}

case class Mod(left: RTerm, right: RTerm) extends BiOperationTerm(left, right) {
  def termType = TermType.MOD
}

case class All(left: RTerm, right: RTerm) extends BiOperationTerm(left, right) {
  def termType = TermType.ALL
}


case class RAny(left: RTerm, right: RTerm) extends BiOperationTerm(left, right) {
  def termType = TermType.ANY
}

