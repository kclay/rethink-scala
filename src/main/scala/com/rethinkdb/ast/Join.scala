package com.rethinkdb.ast

import com.rethinkdb.Term
import ql2.Term.TermType.EnumVal
import ql2.Term.TermType

abstract class Join extends ProduceSequence {
  val left: Table

  def zip = Zip(this)

}

abstract class PredicateJoin extends Join {
  val right: Table

  val func: Predicate2
  override lazy val args: Seq[Term] = buildArgs(left, right, func())
}

/** Returns the inner product of two sequences (e.g. a table, a filter result) filtered by the predicate.
 *  The query compares each row of the left sequence with each row of the right sequence to find all pairs of
 *  rows which satisfy the predicate. When the predicate is satisfied, each matched pair of rows of both
 *  sequences are combined into a result row.
 *  @param left
 *  @param right
 *  @param func
 */
case class InnerJoin(left: Table, right: Table, func: Predicate2) extends PredicateJoin {
  def termType: EnumVal = TermType.INNER_JOIN
}

/** Computes a left outer join by retaining each row in the left table even if no match was found in the right table.
 *  @param left
 *  @param right
 *  @param func
 */
case class OuterJoin(left: Table, right: Table, func: Predicate2) extends PredicateJoin {
  def termType: EnumVal = TermType.OUTER_JOIN
}

/** An efficient join that looks up elements in the right table by primary key.
 *  @param left
 *  @param right
 *  @param attr
 *  @param index
 */
case class EqJoin(left: Table, right: Table, attr: String, index: Option[String] = None) extends Join {
  def termType: EnumVal = TermType.EQ_JOIN

  override lazy val args: Seq[Term] = buildArgs(left, right, attr, index)
}

/** Used to 'zip' up the result of a join by merging the 'right' fields into 'left' fields of each member of the sequence.
 *  @param target
 */
case class Zip(target: Join) extends ProduceSequence {

  override lazy val args: Seq[Term] = buildArgs(target)

  def termType: EnumVal = TermType.ZIP
}