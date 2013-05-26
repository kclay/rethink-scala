package com.rethinkdb.ast


sealed trait Token

trait TermBlock extends Token

trait TokenType extends Token {
  type T

  def name: String

  def value: T
}

case class TermTokenType(termType: ql2.Term.TermType.EnumVal) extends TokenType {
  type T = ql2.Term.TermType.EnumVal

  def name: String = termType.name

  def value = termType
}

case class DatumTokenType(datumType: ql2.Datum.DatumType.EnumVal) extends TokenType {
  type T = ql2.Datum.DatumType.EnumVal

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


