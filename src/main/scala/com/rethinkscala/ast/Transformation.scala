package com.rethinkscala.ast

import com.rethinkscala.Term
import ql2.Term.TermType.EnumVal
import ql2.Term.TermType

abstract class Transformation extends ProduceAnySequence {
  val target: Sequence
  val func: Predicate

  override lazy val args = buildArgs(target, func())

}

/** Transform each element of the sequence by applying the given mapping function.
 *  @param target
 *  @param func
 */
case class RMap(target: Sequence, func: Predicate1) extends Transformation {

  def termType: EnumVal = TermType.MAP

  def toConcat = ConcatMap(target, func)

}

/** Flattens a sequence of arrays returned by the mappingFunction into a single sequence.
 *  @param target
 *  @param func
 */
case class ConcatMap(target: Sequence, func: Predicate1) extends Transformation {

  def termType: EnumVal = TermType.CONCATMAP

  def toMap = RMap(target, func)
}

/** Takes a sequence of objects and a list of fields. If any objects in the sequence don't have all of
 *  the specified fields, they're dropped from the sequence. The remaining objects have the specified fields plucked out.
 *  (This is identical to `has_fields` followed by `pluck` on a sequence.)
 *  @param target
 *  @param fields
 */
case class WithFields(target: Sequence, fields: Seq[String]) extends ProduceAnySequence {

  def termType = TermType.WITH_FIELDS
  override lazy val args = buildArgs(fields.+:(target): _*)
}
case class SliceRange(start: Int = 0, end: Int = -1)

abstract class Ordering extends Term {
  val attr: String
  def flip: Ordering
  override lazy val args: Seq[Term] = buildArgs(attr)
}

case class Asc(attr: String) extends Ordering {

  def flip = Desc(attr)
  def termType: EnumVal = TermType.ASC
}

case class Desc(attr: String) extends Ordering {
  def flip = Asc(attr)
  def termType: EnumVal = TermType.DESC
}

/** Sort the sequence by document values of the given key(s).
 *  order by defaults to ascending ordering. To explicitly specify the ordering, wrap the attribute with either r.asc or r.desc.
 *  @param target
 *  @param keys
 */
case class OrderBy(target: Sequence, keys: Seq[Ordering]) extends ProduceAnySequence {
  def termType: EnumVal = TermType.ORDERBY
}

/** Skip a number of elements from the head of the sequence.
 *  @param target
 *  @param index
 */
case class Skip(target: Sequence, index: Int) extends ProduceAnySequence {
  def termType: EnumVal = TermType.SKIP
}

/** Concatenate two sequences.
 *  @param target
 *  @param others
 */
case class Union(target: Sequence, others: Sequence) extends ProduceAnySequence {

  override lazy val args: Seq[Term] = buildArgs(target, others)

  def termType: EnumVal = TermType.UNION
}

/** Get the nth element of a sequence.
 *  @param target
 *  @param left
 *  @param right
 */
case class Slice(target: Sequence, left: Int = 0, right: Int = -1) extends ProduceAnySequence {
  override lazy val args = buildArgs(target, left, right)

  def termType = TermType.SLICE
}

/** End the sequence after the given number of elements.
 *  @param target
 *  @param amount
 */
case class Limit(target: Sequence, amount: Int) extends ProduceAnySequence {
  override lazy val args = buildArgs(target, amount)

  def termType = TermType.LIMIT
}

/** Test if a sequence is empty.
 *  @param target
 */
case class IsEmpty(target: Sequence) extends ProduceBinary {
  def termType = TermType.IS_EMPTY
}

/** Get the indexes of an element in a sequence. If the argument is a predicate, get the indexes of all elements matching it.
 *  @param target
 *  @param filter
 */
case class IndexesOf(target: Sequence, filter: Either[Datum, BooleanPredicate]) extends ProduceAnySequence {

  override lazy val args = buildArgs(target, filter match {
    case Left(x)  => x
    case Right(f) => f()
  })
  def termType = TermType.INDEXES_OF
}

case class Nth(target: Sequence, index: Int) extends ProduceAny {
  def termType = TermType.NTH
}

/** Select a given number of elements from a sequence with uniform random distribution. Selection is done without replacement.
 *  @param target
 *  @param amount
 */
case class Sample(target: Sequence, amount: Int) extends ProduceAnySequence {
  def termType = TermType.SAMPLE
}