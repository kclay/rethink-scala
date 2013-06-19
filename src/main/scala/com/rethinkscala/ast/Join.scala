package com.rethinkscala.ast

import com.rethinkscala.{ AssocPair, Term }
import ql2.Term.TermType.EnumVal
import ql2.Term.TermType

abstract class Join extends ProduceSequence {
  val left: Sequence
  val right: Sequence

}

abstract class PredicateJoin extends Join {

  val func: BooleanPredicate2
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
case class InnerJoin(left: Sequence, right: Sequence, func: BooleanPredicate2) extends PredicateJoin {
  def termType: EnumVal = TermType.INNER_JOIN
}

/** Computes a left outer join by retaining each row in the left table even if no match was found in the right table.
 *  @param left
 *  @param right
 *  @param func
 */
case class OuterJoin(left: Sequence, right: Sequence, func: BooleanPredicate2) extends PredicateJoin {
  def termType: EnumVal = TermType.OUTER_JOIN
}

/** An efficient join that looks up elements in the right table by primary key.
 *  @param left
 *  @param right
 *  @param attr
 *  @param index
 */
case class EqJoin(left: Sequence, attr: String, right: Sequence, index: Option[String] = None) extends Join {
  def termType: EnumVal = TermType.EQ_JOIN

  override lazy val args: Seq[Term] = buildArgs(left, attr, right)
  override lazy val optargs: Iterable[AssocPair] = buildOptArgs(Map("index" -> index))
}

/** Used to 'zip' up the result of a join by merging the 'right' fields into 'left' fields of each member of the sequence.
 *  @param target
 */
case class Zip(target: Sequence) extends ProduceSequence {

  def termType: EnumVal = TermType.ZIP
}