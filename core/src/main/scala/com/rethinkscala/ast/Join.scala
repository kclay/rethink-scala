package com.rethinkscala.ast

import com.rethinkscala.net.{AbstractCursor, RethinkCursor}
import com.rethinkscala.{AssocPair, Term, ZipResult}
import ql2.Ql2.Term.TermType

abstract class Join[L,R,C[_]] extends ProduceJoin[L,R,C] {

  val left: Sequence[L,C]
  val right:Sequence[R,C]

}

abstract class PredicateJoin[L,R,C[_]] extends Join[L,R,C] {

  val func: ScalaBooleanPredicate2
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
case class InnerJoin[L,R,C[_]](left: Sequence[L,C], right: Sequence[R,C], func: ScalaBooleanPredicate2) extends
PredicateJoin[L,R,C] {
  def termType = TermType.INNER_JOIN
}

/** Computes a left outer join by retaining each row in the left table even if no match was found in the right table.
 *  @param left
 *  @param right
 *  @param func
 */
case class OuterJoin[L,R,C[_]](left: Sequence[L,C], right: Sequence[R,C], func: ScalaBooleanPredicate2) extends
PredicateJoin[L,R,C] {
  def termType = TermType.OUTER_JOIN
}

/** An efficient join that looks up elements in the right table by primary key.
 *  @param left
 *  @param right
 *  @param attrOrFunc
 *  @param index
 */
case class EqJoin[L,R,C[_]](left: Sequence[L,C], attrOrFunc:Either[String,Predicate1], right: Sequence[R,C], index: Option[String] = None)
  extends Join[L,R,C] {
  def termType = TermType.EQ_JOIN

  override lazy val args: Seq[Term] = buildArgs(left, attrOrFunc.fold(identity,identity), right)
  override lazy val optargs: Iterable[AssocPair] = buildOptArgs(Map("index" -> index))
}

/** Used to 'zip' up the result of a join by merging the 'right' fields into 'left' fields of each member of the sequence.
 *  @param target
 */
case class Zip[L,R,C[_]](target: JoinTyped[L,R,C]) extends ProduceSeq[ZipResult[L,R],C] {

  def termType = TermType.ZIP
}