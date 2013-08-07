package com.rethinkscala.ast

sealed trait Token

import ql2.{Ql2 => ql2}

trait TermBlock extends Token

trait TokenType extends Token {
  type T

  def name: String

  def value: T
}

case class TermTokenType(termType: ql2.Term.TermType) extends TokenType {
  type T = ql2.Term.TermType

  def name: String = termType.name

  def value = termType
}

case class DatumTokenType(datumType: ql2.Datum.DatumType) extends TokenType {
  type T = ql2.Datum.DatumType

  def name: String = datumType.name

  def value = datumType
}

trait AssocPairToken[T] extends Token {

  val key: String
  val value: Any
  lazy val token = Expr(value)

  def pair: AnyRef

}

// implicit def tokenTypeToTermType(tokenType:Either[p.TermNode.TermType,p.Datum.DatumType]):p.TermNode.TermType =tokenType.left

