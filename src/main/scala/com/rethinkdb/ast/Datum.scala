package com.rethinkdb.ast

import ql2.{Ql2=>p}

import com.rethinkdb.{ExprWrap, Composable, Term}
import com.rethinkdb.conversions.Tokens._

sealed trait Datum extends Term with ExprWrap with Composable {

  override def compile(builder: p.Term.Builder) = {
    builder.setType(p.Term.TermType.DATUM)
    build(builder.getDatumBuilder)


  }

  override def optArgsBuilder(key: String, value: Any): AssocPairToken = DatumAssocPairToken(key, value)

  def termType: TokenType = p.Term.TermType.DATUM

  def datumType: TokenType

  def build(builder: p.Datum.Builder)

}

object Datum {

  def apply(a: Any): Datum = a match {

    case Some(v) => Datum(v)
    case s: String => StringDatum(s)
    case i: Int => NumberDatum(i)
    case f: Float => NumberDatum(f)
    case l: Long => NumberDatum(l)
    case b: Boolean => BooleanDatum(b)

  }
}


object NoneDatum {
  def apply() = new NoneDatum()
}

class NoneDatum extends Datum {

  def datumType: DatumTokenType = p.Datum.DatumType.R_NULL

  def build(builder: p.Datum.Builder) = {

  }
}

case class BooleanDatum(value: Boolean) extends Datum {

  def datumType: DatumTokenType = p.Datum.DatumType.R_BOOL

  def build(builder: p.Datum.Builder) = {
    builder.setType(p.Datum.DatumType.R_BOOL).setRBool(value)
  }
}

case class NumberDatum(value: Double) extends Datum {
  def datumType: DatumTokenType = p.Datum.DatumType.R_NUM

  def build(builder: p.Datum.Builder) = {

    builder.setType(p.Datum.DatumType.R_NUM).setRNum(value)
  }
}

case class StringDatum(value: String) extends Datum {
  def datumType: DatumTokenType = p.Datum.DatumType.R_STR

  def build(builder: p.Datum.Builder) = {

    builder.setType(p.Datum.DatumType.R_STR).setRStr(value)
  }
}