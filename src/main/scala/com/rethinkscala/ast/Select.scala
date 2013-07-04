package com.rethinkscala.ast

import ql2.Term.TermType
import ql2.Term.TermType.EnumVal

case class Get(target: Table, attribute: Any) extends ProduceSingleSelection {

  def termType = TermType.GET
}

case class GetAll(target: Table, attr: String, index: Option[String] = None) extends ProduceArray {

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

  def termType: EnumVal = TermType.FILTER
}