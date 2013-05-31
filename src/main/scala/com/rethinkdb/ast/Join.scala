package com.rethinkdb.ast

import com.rethinkdb.Term
import ql2.Term.TermType.EnumVal
import ql2.Term.TermType

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 5/30/13
 * Time: 8:49 PM
 * To change this template use File | Settings | File Templates.
 */
trait WithJoin {
  self: Table =>


  def <<(other: Table, func: Predicate2) = innerJoin(other, func)

  def innerJoin(other: Table, func: Predicate2) = InnerJoin(this, other, func)

  def outerJoin(other: Table, func: Predicate2) = OuterJoin(this, other, func)

  def >>(other: Table, func: Predicate2) = outerJoin(other, func)

  def >>=(attr: String, other: Table, index: Option[String] = None) = eqJoin(attr, other, index)

  def eqJoin(attr: String, other: Table, index: Option[String] = None) = EqJoin(this, other, attr, index)

  def zip()
}

abstract class Join extends ProduceSequence  {
  val left: Table

}

abstract class PredicateJoin extends Join {
  val right: Table

  val func: Predicate2
  override lazy val args: Seq[Term] = buildArgs(left, right, func())
}

case class InnerJoin(left: Table, right: Table, func: Predicate2) extends PredicateJoin {
  def termType: EnumVal = TermType.INNER_JOIN
}

case class OuterJoin(left: Table, right: Table, func: Predicate2) extends PredicateJoin {
  def termType: EnumVal = TermType.OUTER_JOIN
}


case class EqJoin(left: Table, right: Table, attr: String, index: Option[String] = None) extends Join {
  def termType: EnumVal = TermType.EQ_JOIN

  override lazy val args: Seq[Term] = buildArgs(left, right, attr, index)
}