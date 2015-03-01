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


case class InsertExpr(private val value:(Any,Boolean)) extends AnyVal
case class Insert[T<:AnyRef, R<:AnyRef](table: Table[T], records: Either[Seq[Map[String, Any]], Seq[R]],
                                                options: InsertOptions,writeNulls:Boolean=false)
  extends WithLifecycle[Seq[String]] with ProduceDocument[InsertResult] {


  override lazy val args = buildArgs(table, records match {
    case Left(x: Seq[Map[String, Any]]) => x
    case Right(Seq(doc)) => Json(Reflector.toJson(doc))
    case Right(x: Seq[R]) => Json(Reflector.toJson(x))


  })


  private[this] def toMap(doc: AnyRef): Map[String, Any] = Expr.mapForInsert(doc,writeNulls)

  def withNulls = copy(writeNulls = true)

  private[rethinkscala] lazy val argsForJson = buildArgs(table, records match {
    case Left(x: Seq[Map[String, Any]]) => x
    case Right(Seq(doc)) => MakeObj(toMap(doc)) // wrap in single object so withResults work
    case Right(x: Seq[R]) => MakeArray(x.map(toMap(_)))


  })
  override lazy val optargs = buildOptArgs(options.toMap)

  @deprecated("use .withChanges", "0.4.5")
  def withResults = withChanges

  def withChanges:Insert[T,R] = copy(options = options.copy(returnValues = None, returnChanges = Some(true)))

  def termType:TermType = TermType.INSERT


  private def lifecycle(f: (Document, Int) => Unit) = records match {
    case Right(x) => x.zipWithIndex foreach  {

      case (d, i) if d.isInstanceOf[Document] => f(d.asInstanceOf[Document], i)

    }
    case _ =>
  }

  override protected def after = lifecycle((d, i) => d.invokeAfterInsert)

  override protected def before = lifecycle((d, i) => d.invokeBeforeInsert)

  override protected def after(values: Seq[String]) = lifecycle((d, i) => d.invokeAfterInsert(values(i)))
}

case class Update[T](target: Selection[T], wrap: FuncWrap,
                     options: UpdateOptions)
  extends ProduceDocument[ChangeResult] {

  override lazy val args = buildArgs(target, wrap)
  override lazy val optargs = buildOptArgs(options.toMap)

  def termType:TermType = TermType.UPDATE

  @deprecated("use .withChanges", "0.4.5")
  def withResults = withChanges

  def withChanges:Update[T] = copy(options = options.copy(returnValues = None, returnChanges = Some(true)))
}

case class Replace[T](target: Selection[_], wrap: FuncWrap,
                      options: UpdateOptions)
  extends ProduceDocument[ChangeResult] {

  override lazy val args = buildArgs(target, wrap)
  override lazy val optargs = buildOptArgs(options.toMap)

  def termType:TermType = TermType.REPLACE

  @deprecated("use .withChanges", "0.4.5")
  def withResults = withChanges

  def withChanges:Replace[T] = copy(options = options.copy(returnValues = None, returnChanges = Some(true)))

}

case class Delete[T](target: Selection[T], durability: Option[Durability.Kind] = None) extends ProduceDocument[ChangeResult] {


  override lazy val args = buildArgs(target)
  override lazy val optargs = buildOptArgs(Map("durability" -> durability))

  def termType:TermType = TermType.DELETE
}
