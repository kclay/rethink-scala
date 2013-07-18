package com.rethinkscala.ast

import ql2.Term.TermType
import com.rethinkscala.reflect.Reflector
import com.rethinkscala.net.{ChangeResult, Document, InsertResult}


sealed trait Lifecycle

case object Before extends Lifecycle

case class After(value: Option[Any]) extends Lifecycle

abstract class WithLifecycle[R](implicit mf: Manifest[R]) {


  private[rethinkscala] def apply(lc: Lifecycle) = lc match {
    case Before => before
    case After(v) => v.map {
      x => if (mf.runtimeClass isAssignableFrom x.getClass) after(x.asInstanceOf[R])
    }
  }

  protected def before = {}

  protected def after(values: R) = {}
}

case class Insert[T <: Document](table: Table[T], records: Either[Seq[Map[String, Any]], Seq[T]],
                                 upsert: Option[Boolean] = None, durability: Option[Durability.Kind] = None, returnValues: Option[Boolean] = None)
  extends WithLifecycle[Seq[String]] with ProduceDocument[InsertResult] {


  override lazy val args = buildArgs(table, records match {
    case Left(x: Seq[Map[String, Any]]) => x
    case Right(x: Seq[T]) => Json(Reflector.toJson(if (x.size > 1) x else x.head))


  })
  override lazy val optargs = buildOptArgs(Map("upsert" -> upsert, "durability" -> durability, "return_vals" -> returnValues))

  def withResults = Insert[T](table, records, upsert, durability, Some(true))

  def termType = TermType.INSERT


  private def lifecycle(f: (T, Int) => Unit) = records match {
    case Right(x) => x.zipWithIndex map {

      case (d, i) => f(d, i)
    }
    case _ =>
  }

  override protected def before = lifecycle((d, i) => d.beforeInsert)

  override protected def after(values: Seq[String]) = lifecycle((d, i) => d.afterInsert(values(i)))
}

case class Update(target: Selection, data: Either[Map[String, Any], Predicate],
                  durability: Option[Durability.Kind] = None, nonAtomic: Option[Boolean] = None, returnValues: Option[Boolean] = None)
  extends ProduceDocument[ChangeResult] {

  override lazy val args = buildArgs(target, data match {
    case Left(x: Map[String, Any]) => x
    case Right(x: Predicate) => x()
  })
  override lazy val optargs = buildOptArgs(Map("non_atomic" -> nonAtomic, "durability" -> durability, "return_vals" -> returnValues))

  def termType = TermType.UPDATE

  def withResults = Update(target, data, durability, nonAtomic, Some(true))
}

case class Replace(target: Selection, data: Either[Map[String, Any], Predicate1],
                   durability: Option[Durability.Kind] = None, nonAtomic: Option[Boolean] = None, returnValues: Option[Boolean] = None)
  extends ProduceDocument[ChangeResult] {

  override lazy val args = buildArgs(target, data match {
    case Left(x: Map[String, Any]) => x
    case Right(x: Predicate1) => x()
  })
  override lazy val optargs = buildOptArgs(Map("non_atomic" -> nonAtomic, "durability" -> durability, "return_vals" -> returnValues))

  def termType = TermType.REPLACE

  def withResults = Replace(target, data, durability, nonAtomic, Some(true))

}

case class Delete(target: Selection, durability: Option[Durability.Kind] = None) extends ProduceDocument[ChangeResult] {


  override lazy val args = buildArgs(target)
  override lazy val optargs = buildOptArgs(Map("durability" -> durability))

  def termType = TermType.DELETE
}
