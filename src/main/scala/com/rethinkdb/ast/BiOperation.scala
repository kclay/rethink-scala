package com.rethinkdb.ast

import com.rethinkdb.{Composable, BiOpTerm, Term}
import ql2.{Ql2=>p}
import com.rethinkdb.conversions.Tokens._

/**
 * Created with IntelliJ IDEA.                                                                n
 * User: keyston
 * Date: 4/2/13
 * Time: 7:34 PM
 * To change this template use File | Settings | File Templates.
 */
abstract class BiOperationTerm(left: Term, right: Term) extends BiOpTerm {
  override lazy val args = buildArgs(left, right)
}

case class Eq(left: Term, right: Term) extends BiOperationTerm(left, right) {
  def termType: TokenType = p.Term.TermType.EQ
}

case class Ne(left: Term, right: Term) extends BiOperationTerm(left, right) {
  def termType: TokenType = p.Term.TermType.NE
}

case class Lt(left: Term, right: Term) extends BiOperationTerm(left, right) {
  def termType: TokenType = p.Term.TermType.LT
}

case class Le(left: Term, right: Term) extends BiOperationTerm(left, right) {
  def termType: TokenType = p.Term.TermType.LE
}

case class Gt(left: Term, right: Term) extends BiOperationTerm(left, right) {
  def termType: TokenType = p.Term.TermType.GT
}


case class Ge(left: Term, right: Term) extends BiOperationTerm(left, right) {
  def termType: TokenType = p.Term.TermType.GE
}

case class Not(prev: Term) extends Term with Composable {
  override lazy val args = buildArgs(prev)

  def termType: TokenType = p.Term.TermType.NOT
}

case class Add(left: Term, right: Term) extends BiOperationTerm(left, right) {
  def termType: TokenType = p.Term.TermType.ADD
}

case class Sub(left: Term, right: Term) extends BiOperationTerm(left, right) {
  def termType: TokenType = p.Term.TermType.SUB
}

case class Mul(left: Term, right: Term) extends BiOperationTerm(left, right) {
  def termType: TokenType = p.Term.TermType.MUL
}

case class Div(left: Term, right: Term) extends BiOperationTerm(left, right) {
  def termType: TokenType = p.Term.TermType.DIV
}

case class Mod(left: Term, right: Term) extends BiOperationTerm(left, right) {
  def termType: TokenType = p.Term.TermType.MOD
}
case class All(left: Term, right: Term) extends BiOperationTerm(left, right) {
  def termType: TokenType = p.Term.TermType.ALL
}

case class RAny(left: Term, right: Term) extends BiOperationTerm(left, right) {
  def termType: TokenType = p.Term.TermType.ANY
}

