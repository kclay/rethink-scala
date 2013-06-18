package com.rethinkscala.ast

import com.rethinkscala._
import ql2.Term.TermType
import com.rethinkscala.InsertResult
import com.rethinkscala.ast.Table
import scala.Some
import com.rethinkscala.reflect.Reflector

case class Insert(table: Table, records: Either[Seq[Map[String, Any]], Seq[Document]],
                  upsert: Option[Boolean] = None, durability: Option[String] = None) extends TermMessage
    with ProduceDocument

    with DocumentConversion[InsertResult] {

  override lazy val args = buildArgs(table, records match {
    case Left(x: Seq[Map[String, Any]]) => x
    case Right(x: Seq[Document])        => x.map(Reflector.toMap(_))
  })
  override lazy val optargs = buildOptArgs(Map("upsert" -> upsert, "durability" -> durability))

  def termType = TermType.INSERT

  implicit def optionInt2Int(i: Option[Any]) = i.get.asInstanceOf[Int]

  implicit def optionAny2OptionString(i: Option[Any]) = i.asInstanceOf[Option[String]]

  implicit def optionAny2Seq(i: Option[Any]) = i match {
    case Some(errors: Seq[Any]) => errors
    case _                      => Seq.empty[Any]
  }

  def convert(value: Map[String, Any]): InsertResult = InsertResult(
    value.get("inserted"), value.get("replaced"), value.get("unchanged"), value.get("errors"), value.get("first_error"),
    value.get("generated_keys"), value.get("deleated"), value.get("skipped")

  )

}

case class Update(target: Sequence, data: Either[Map[String, Any], Predicate]) extends ProduceDocument {

  override lazy val args = buildArgs(target, data match {
    case Left(x: Map[String, Any]) => x
    case Right(x: Predicate)       => x()
  })

  def termType = TermType.UPDATE
}
