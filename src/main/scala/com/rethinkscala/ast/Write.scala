package com.rethinkscala.ast

import com.rethinkscala._
import ql2.Term.TermType
import com.rethinkscala.InsertResult
import com.rethinkscala.reflect.Reflector

case class Insert(table: Table, records: Either[Seq[Map[String, Any]], Seq[Document]],
                  upsert: Option[Boolean] = None, durability: Option[Durability.Kind] = None)
    extends ProduceDocument[InsertResult] {

  override lazy val args = buildArgs(table, records match {
    case Left(x: Seq[Map[String, Any]]) => x
    case Right(x: Seq[Document])        => x.map(Reflector.toMap(_))
  })
  override lazy val optargs = buildOptArgs(Map("upsert" -> upsert, "durability" -> durability))

  def termType = TermType.INSERT

}

case class Update(target: Selection, data: Either[Map[String, Any], Predicate],
                  durability: Option[Durability.Kind] = None, nonAtomic: Option[Boolean] = None)
    extends ProduceDocument[UpdateResult] {

  override lazy val args = buildArgs(target, data match {
    case Left(x: Map[String, Any]) => x
    case Right(x: Predicate)       => x()
  })
  override lazy val optargs = buildOptArgs(Map("non_atomic" -> nonAtomic, "durability" -> durability))

  def termType = TermType.UPDATE
}

case class Replace(target: Selection, data: Either[Map[String, Any], Predicate1],
                   durability: Option[Durability.Kind] = None, nonAtomic: Option[Boolean] = None)
    extends ProduceDocument[ReplaceResult] {

  override lazy val args = buildArgs(target, data match {
    case Left(x: Map[String, Any]) => x
    case Right(x: Predicate1)      => x()
  })
  override lazy val optargs = buildOptArgs(Map("non_atomic" -> nonAtomic, "durability" -> durability))

  def termType = TermType.REPLACE

}
