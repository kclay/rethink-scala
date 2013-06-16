package com.rethinkscala.ast

import com.rethinkscala.{InsertResult, DocumentConversion, TermMessage}
import ql2.Term.TermType
import com.rethinkscala.Document


case class Insert(table: Table, records: Either[Seq[Map[String, Any]],Seq[Document]],
                  upsert: Option[Boolean] = None) extends TermMessage
                                                                                                        with ProduceDocument

                                                                                                        with DocumentConversion[InsertResult] {

  override lazy val args = buildArgs(table, records)
  override lazy val optargs = buildOptArgs(Map("upsert" -> upsert))

  def termType = TermType.INSERT

  implicit def optionInt2Int(i: Option[Any]) = i.get.asInstanceOf[Int]

  implicit def optionAny2OptionString(i: Option[Any]) = i.asInstanceOf[Option[String]]

  implicit def optionAny2Seq(i: Option[Any]) = i match {
    case Some(errors: Seq[Any]) => errors
    case _ => Seq.empty[Any]
  }

  def convert(value: Map[String, Any]): InsertResult = InsertResult(
    value.get("inserted"), value.get("replaced"), value.get("unchanged"), value.get("errors"), value.get("first_error"),
    value.get("generated_keys"), value.get("deleated"), value.get("skipped")

  )

}
