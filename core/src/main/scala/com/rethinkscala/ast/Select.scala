package com.rethinkscala.ast

import ql2.Ql2.Term.TermType
import com.rethinkscala.{BetweenOptions, Document}

case class Get[R <: Document](target: Table[R], attribute: Any) extends ProduceSingleSelection[R] {

  def termType = TermType.GET
}

case class GetAll[R <: Document](target: Table[R], attr: Seq[Any], index: Option[String] = None) extends ProduceArray[R] {

  override lazy val optargs = buildOptArgs(Map("index" -> index))
  override lazy val args = buildArgs((attr.+:(target)):_*)

  def termType = TermType.GET_ALL
}

case class Between[T](target: StreamSelection[T], start: Literal, end: Literal, options:BetweenOptions) extends ProduceStreamSelection[T] {
  override lazy val args = buildArgs(target, start, end)

  override lazy val optargs = buildOptArgs(options.toMap)

  def termType = TermType.BETWEEN
}

case class Filter[T](target: Filterable[T], wrap: FuncWrap, default: Option[Boolean] = None) extends ProduceStreamSelection[T] {

  override lazy val args = buildArgs(target, wrap)
  override lazy val optargs = buildOptArgs(Map("default" -> default))

  def termType = TermType.FILTER
}