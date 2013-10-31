package com.rethinkscala.ast

import ql2.Ql2.Term.TermType
import com.rethinkscala.reflect.Reflector
import com.rethinkscala._
import com.rethinkscala.InsertOptions
import com.rethinkscala.UpdateOptions
import scala.Some


sealed trait Lifecycle

case object Before extends Lifecycle

case class After(value: Option[Any]) extends Lifecycle

abstract class WithLifecycle[R](implicit mf: Manifest[R]) {


  private[rethinkscala] def apply(lc: Lifecycle) = lc match {
    case Before => before
    case After(v) => v match {
      case Some(x) => if (mf.runtimeClass isAssignableFrom x.getClass) after(x.asInstanceOf[R])
      case _ => after
    }
  }

  protected def before = {}

  protected def after = {}

  protected def after(values: R) = {}
}

case class Insert[T <: Document](table: Table[T], records: Either[Seq[Map[String, Any]], Seq[T]],
                                 options: InsertOptions)
  extends WithLifecycle[Seq[String]] with ProduceDocument[InsertResult] {


  override lazy val args = buildArgs(table, records match {
    case Left(x: Seq[Map[String, Any]]) => x
    case Right(x: Seq[T]) => Json(Reflector.toJson(if (x.size > 1) x else x.head))


  })
  override lazy val optargs = buildOptArgs(options.toMap)

  def withResults = Insert[T](table, records, options.copy(returnValues = Some(true)))

  def termType = TermType.INSERT


  private def lifecycle(f: (T, Int) => Unit) = records match {
    case Right(x) => x.zipWithIndex map {

      case (d, i) => f(d, i)
    }
    case _ =>
  }

  override protected def after = lifecycle((d, i) => d.invokeAfterInsert)

  override protected def before = lifecycle((d, i) => d.invokeBeforeInsert)

  override protected def after(values: Seq[String]) = lifecycle((d, i) => d.invokeAfterInsert(values(i)))
}

case class Update[T](target: Selection[T], data: Either[Typed, Predicate],
                     options: UpdateOptions)
  extends ProduceDocument[ChangeResult] {

  override lazy val args = buildArgs(target, data match {
    case Left(x: Typed) => FuncWrap(x)
    case Right(x: Predicate) => x()
  })
  override lazy val optargs = buildOptArgs(options.toMap)

  def termType = TermType.UPDATE

  def withResults = Update(target, data, options.copy(returnValues = Some(true)))
}

case class Replace[T](target: Selection[T], data: Either[Map[String, Any], Predicate1],
                      options: UpdateOptions)
  extends ProduceDocument[ChangeResult] {

  override lazy val args = buildArgs(target, data match {
    case Left(x: Map[String, Any]) => x
    case Right(x: Predicate1) => x()
  })
  override lazy val optargs = buildOptArgs(options.toMap)

  def termType = TermType.REPLACE

  def withResults = Replace(target, data, options.copy(returnValues = Some(true)))

}

case class Delete[T](target: Selection[T], durability: Option[Durability.Kind] = None) extends ProduceDocument[ChangeResult] {


  override lazy val args = buildArgs(target)
  override lazy val optargs = buildOptArgs(Map("durability" -> durability))

  def termType = TermType.DELETE
}
