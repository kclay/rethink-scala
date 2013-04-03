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
abstract class BiOperationTerm(left: Datum, right : Datum) extends BiOpTerm {
  override lazy val args = buildArgs(left, right)
}

case class Eq(left: Datum, right : Datum) extends BiOperationTerm(left, right) {
  def termType: TokenType = p.Term.TermType.EQ
}

case class Ne(left: Datum, right : Datum) extends BiOperationTerm(left, right) {
  def termType: TokenType = p.Term.TermType.NE
}

case class Lt(left: Datum, right : Datum) extends BiOperationTerm(left, right) {
  def termType: TokenType = p.Term.TermType.LT
}

case class Le(left: Datum, right : Datum) extends BiOperationTerm(left, right) {
  def termType: TokenType = p.Term.TermType.LE
}

case class Gt(left: Datum, right : Datum) extends BiOperationTerm(left, right) {
  def termType: TokenType = p.Term.TermType.GT
}


case class Ge(left: Datum, right : Datum) extends BiOperationTerm(left, right) {
  def termType: TokenType = p.Term.TermType.GE
}

case class Not(prev: Datum) extends Term with Composable {
  override lazy val args = buildArgs(prev)

  def termType: TokenType = p.Term.TermType.NOT
}

case class Add(left: Datum, right : Datum) extends BiOperationTerm(left, right) {
  def termType: TokenType = p.Term.TermType.ADD
}

case class Sub(left: Datum, right : Datum) extends BiOperationTerm(left, right) {
  def termType: TokenType = p.Term.TermType.SUB
}

case class Mul(left: Datum, right : Datum) extends BiOperationTerm(left, right) {
  def termType: TokenType = p.Term.TermType.MUL
}

case class Div(left: Datum, right : Datum) extends BiOperationTerm(left, right) {
  def termType: TokenType = p.Term.TermType.DIV
}

case class Mod(left: Datum, right : Datum) extends BiOperationTerm(left, right) {
  def termType: TokenType = p.Term.TermType.MOD
}
case class All(left: Datum, right : Datum) extends BiOperationTerm(left, right) {
  def termType: TokenType = p.Term.TermType.ALL
}


case class RAny(left: Datum, right : Datum) extends BiOperationTerm(left, right) {
  def termType: TokenType = p.Term.TermType.ANY
}

