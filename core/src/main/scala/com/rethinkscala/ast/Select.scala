package com.rethinkscala.ast

import ql2.Ql2.Term.TermType
import com.rethinkscala.net.Document

case class Get[R <: Document](target: Table[R], attribute: Any) extends ProduceTypedSingleSelection[R] {

  def termType = TermType.GET
}

case class GetAll[R <: Document](target: Table[R], attr: String, index: Option[String] = None) extends ProduceTypedArray[R] {

  override lazy val optargs = buildOptArgs(Map("index" -> index))
  override lazy val args = buildArgs(target, attr)

  def termType = TermType.GET_ALL
}

case class Between[T](target: StreamSelection[T], start: Literal, end: Literal, index: Option[String] = None) extends ProduceTypedStreamSelection[T]{
  override lazy val args = buildArgs(target, start, end)

  override lazy val optargs = buildOptArgs(Map("index" -> index))

  def termType = TermType.BETWEEN
}

case class Filter[T](target: Sequence[T], filter: Either[Map[String, Any], Predicate1]) extends ProduceTypedStreamSelection[T]{

  override lazy val args = buildArgs(target, filter match {
    case Left(x) => x
    case Right(x) => x()
  })

  def termType = TermType.FILTER
}