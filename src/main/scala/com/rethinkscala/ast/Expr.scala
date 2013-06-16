package com.rethinkscala.ast

import com.rethinkscala.Term
import com.rethinkscala.Document
import com.rethinkscala.relect.Reflector

object Expr {

  def apply(term: Term): Term = term

  def apply(value: Seq[Any]): MakeArray = MakeArray(value)

  def apply(value: Map[String, Any]): MakeObj = MakeObj(value)

  def apply(value: String): StringDatum = StringDatum(value)

  def apply(b: Boolean): BooleanDatum = BooleanDatum(b)

  def apply(i: Int): NumberDatum = NumberDatum(i)

  def apply(l: Long): NumberDatum = NumberDatum(l)

  def apply(f: Float): NumberDatum = NumberDatum(f)

  def apply(d:Document) = MakeObj(Reflector.toMap(d))

  def apply(a: Any): Term = {
    val b = a
    a match {
      case t: Term      => t
      case s: Seq[_]    => MakeArray(s)
      case m: Map[_, _] => MakeObj(m.asInstanceOf[Map[String, Option[Any]]])
      case d:Document => apply(d)
      case a: Any       => Datum(a)

    }
  }

}