package com.rethinkscala.ast

import ql2.Ql2.Term.TermType
import com.rethinkscala.net.Document

case class Get[R <:Document](target: Table[R], attribute: Any) extends ProduceTypedSingleSelection[R] {

  def termType = TermType.GET
}

case class GetAll[R <:Document](target: Table[R], attr: String, index: Option[String] = None) extends ProduceTypedArray[R] {

  override lazy val optargs = buildOptArgs(Map("index" -> index))
  override lazy val args = buildArgs(target, attr)

  def termType = TermType.GET_ALL
}

case class Between(target: StreamSelection, start: Literal, end: Literal, index: Option[String] = None) extends ProduceStreamSelection {
  override lazy val args = buildArgs(target, start, end)

  override lazy val optargs = buildOptArgs(Map("index" -> index))

  def termType = TermType.BETWEEN
}

case class Filter(target: Sequence, filter: Either[MakeObj, Predicate1]) extends ProduceAnySequence {

  override lazy val args = buildArgs(target, filter match {
    case Left(x)  => x
    case Right(x) => x()
  })

  def termType= TermType.FILTER
}