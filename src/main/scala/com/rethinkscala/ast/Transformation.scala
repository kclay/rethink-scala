package com.rethinkscala.ast

import com.rethinkscala.Term
import ql2.Term.TermType.EnumVal
import ql2.Term.TermType

case class SliceRange(start: Int = 0, end: Int = -1)

abstract class Ordering extends Term {
  val attr: String
  override lazy val args: Seq[Term] = buildArgs(attr)
}

case class Asc(attr: String) extends Ordering {
  def termType: EnumVal = TermType.ASC
}

case class Desc(attr: String) extends Ordering {
  def termType: EnumVal = TermType.DESC
}

/** Sort the sequence by document values of the given key(s).
 *  order by defaults to ascending ordering. To explicitly specify the ordering, wrap the attribute with either r.asc or r.desc.
 *  @param target
 *  @param keys
 */
case class OrderBy(target: Json, keys: Seq[Ordering]) extends ProduceSequence {
  def termType: EnumVal = TermType.ORDERBY
}

/** Skip a number of elements from the head of the sequence.
 *  @param target
 *  @param index
 */
case class Skip(target: Sequence, index: Int) extends ProduceSequence {
  def termType: EnumVal = TermType.SKIP
}

/** Concatenate two sequences.
 *  @param target
 *  @param others
 */
case class Union(target: Sequence, others: Sequence) extends ProduceSequence {

  override lazy val args: Seq[Term] = buildArgs(target, others)

  def termType: EnumVal = TermType.UNION
}

/** Get the nth element of a sequence.
 *  @param target
 *  @param left
 *  @param right
 */
case class Slice(target: Sequence, left: Int, right: Int) extends ProduceSequence {
  override lazy val args = buildArgs(target, left, right)

  def termType = TermType.SLICE
}

/** End the sequence after the given number of elements.
 *  @param target
 *  @param amount
 */
case class Limit(target: Sequence, amount: Int) extends ProduceSequence {
  override lazy val args = buildArgs(target, amount)

  def termType = TermType.LIMIT
}
