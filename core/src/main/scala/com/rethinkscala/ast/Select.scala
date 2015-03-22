package com.rethinkscala.ast

import ql2.Ql2.Term.TermType
import com.rethinkscala.{BetweenOptions, Document}

case class Get[R<:AnyRef](target: Table[R], attribute: Typed) extends ProduceSingleDocumentSelection[R] {

  def termType:TermType = TermType.GET
}

case class GetAll[R<:AnyRef ](target: Table[R], attr: Seq[Any], index: Option[String] = None) extends ProduceArray[R] {

  override lazy val optargs = buildOptArgs(Map("index" -> index))
  override lazy val args = buildArgs((attr.+:(target)):_*)

  def withIndex(index:String) = copy(index=Some(index))

  def termType:TermType = TermType.GET_ALL
}

case class Between[T,C[_]](target: StreamSelection[T,C], start: Typed, end:Typed, options:BetweenOptions) extends ProduceStreamSelection[T,C] {
  override lazy val args = buildArgs(target, start, end)

  override lazy val optargs = buildOptArgs(options.toMap)

  def termType:TermType = TermType.BETWEEN
}

case class Filter[T,C[_]](target: Filterable[T,C], wrap: FuncWrap, default: Option[Typed] = None) extends ProduceStreamSelection[T,C] {

  override lazy val args = buildArgs(target, wrap)
  override lazy val optargs = buildOptArgs(Map("default" -> default))

  def termType:TermType = TermType.FILTER
}