package com.rethinkscala.ast

import com.rethinkscala.{ AssocPair, Term }

import ql2.Ql2.Term.TermType
import com.rethinkscala.net.JoinResult

abstract class Join[L,R] extends ProduceSequence[JoinResult[L,R]] {
  val left: Sequence[L]
  val right:Sequence[R]

}

abstract class PredicateJoin[L,R] extends Join[L,R] {

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
case class InnerJoin[L,R](left: Sequence[L], right: Sequence[R], func: BooleanPredicate2) extends PredicateJoin[L,R] {
  def termType = TermType.INNER_JOIN
}

/** Computes a left outer join by retaining each row in the left table even if no match was found in the right table.
 *  @param left
 *  @param right
 *  @param func
 */
case class OuterJoin[L,R](left: Sequence[L], right: Sequence[R], func: BooleanPredicate2) extends PredicateJoin[L,R] {
  def termType = TermType.OUTER_JOIN
}

/** An efficient join that looks up elements in the right table by primary key.
 *  @param left
 *  @param right
 *  @param attr
 *  @param index
 */
case class EqJoin[L,R](left: Sequence[L], attr: String, right: Sequence[R], index: Option[String] = None) extends Join[L,R] {
  def termType = TermType.EQ_JOIN

  override lazy val args: Seq[Term] = buildArgs(left, attr, right)
  override lazy val optargs: Iterable[AssocPair] = buildOptArgs(Map("index" -> index))
}

/** Used to 'zip' up the result of a join by merging the 'right' fields into 'left' fields of each member of the sequence.
 *  @param target
 */
case class Zip[T](target: Sequence[T]) extends ProduceSequence[T] {

  def termType = TermType.ZIP
}