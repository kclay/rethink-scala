package com.rethinkscala.ast

import com.rethinkscala.{BoundOptions, Term}
import ql2.Ql2.Term.TermType

abstract class Transformation[T] extends ProduceSequence[T] {
  val target: Sequence[_]
  val func: FuncWrap

  override lazy val args = buildArgs(target, func())

}

/** Transform each element of the sequence by applying the given mapping function.
  * @param target
  * @param func
  */
case class RMap[T](target: Sequence[_], func: FuncWrap) extends Transformation[T] {

  def termType = TermType.MAP

  def toConcat = ConcatMap(target, func)

}

/** Flattens a sequence of arrays returned by the mappingFunction into a single sequence.
  * @param target
  * @param func
  */
case class ConcatMap[T](target: Sequence[T], func: FuncWrap) extends Transformation[T] {

  def termType = TermType.CONCATMAP

  def toMap = RMap(target, func)
}

/** Takes a sequence of objects and a list of fields. If any objects in the sequence don't have all of
  * the specified fields, they're dropped from the sequence. The remaining objects have the specified fields plucked out.
  * (This is identical to `has_fields` followed by `pluck` on a sequence.)
  * @param target
  * @param fields
  */
case class WithFields[T](target: Sequence[T], fields: Seq[Any]) extends ProduceSequence[T] {

  def termType = TermType.WITH_FIELDS

  override lazy val args = buildArgs(fields.+:(target): _*)
}

case class SliceRange(start: Int = 0, end: Int = -1)


trait Order

abstract class Ordering extends Term with Order {
  val attr: String

  def flip: Ordering

  override lazy val args: Seq[Term] = buildArgs(attr)
}

case class Asc(attr: String) extends Ordering {

  def flip = Desc(attr)

  def termType = TermType.ASC
}

case class Desc(attr: String) extends Ordering {
  def flip = Asc(attr)

  def termType = TermType.DESC
}

/** Sort the sequence by document values of the given key(s).
  * order by defaults to ascending ordering. To explicitly specify the ordering, wrap the attribute with either r.asc or r.desc.
  * @param target
  * @param values
  */
case class OrderBy[T](target: Sequence[T], values: Seq[Order], index: Option[Order] = None) extends ProduceSequence[T] {


  override lazy val args = buildArgs(values.+:(target): _*)

  def termType = TermType.ORDERBY

  def index(i: Order) = OrderBy(target, values, Some(i))
}

/** Skip a number of elements from the head of the sequence.
  * @param target
  * @param index
  */
case class Skip[T](target: Sequence[T], index: Int) extends ProduceSequence[T] {
  def termType = TermType.SKIP
}

/** Concatenate two sequences.
  * @param target
  * @param others
  */
case class Union(target: Sequence[_], others: Sequence[_]) extends ProduceAnySequence {

  override lazy val args: Seq[Term] = buildArgs(target, others)

  def termType = TermType.UNION
}

/** Get the nth element of a sequence.
  * @param target
  * @param left
  * @param right
  */
case class Slice[T](target: Sequence[T], left: Int = 0, right: Int = -1, bounds: BoundOptions) extends ProduceSequence[T] {
  override lazy val args = buildArgs(target, left, right)

  override lazy val optargs = buildOptArgs(bounds.toMap)

  def termType = TermType.SLICE
}

/** End the sequence after the given number of elements.
  * @param target
  * @param amount
  */
case class Limit[T](target: Sequence[T], amount: Int) extends ProduceSequence[T] {
  override lazy val args = buildArgs(target, amount)

  def termType = TermType.LIMIT
}

/** Test if a sequence is empty.
  * @param target
  */
case class IsEmpty[T](target: Sequence[T]) extends ProduceBinary {
  def termType = TermType.IS_EMPTY
}

/** Get the indexes of an element in a sequence. If the argument is a predicate, get the indexes of all elements matching it.
  * @param target
  * @param filter
  */
case class IndexesOf[T](target: Sequence[T], filter: Either[Datum, BooleanPredicate]) extends ProduceSequence[Long] {

  override lazy val args = buildArgs(target, filter match {
    case Left(x) => x
    case Right(f) => f()
  })

  def termType = TermType.INDEXES_OF
}

case class Nth[T](target: Sequence[T], index: Int) extends Produce[T] {
  def termType = TermType.NTH
}

/** Select a given number of elements from a sequence with uniform random distribution. Selection is done without replacement.
  * @param target
  * @param amount
  */
case class Sample[T](target: Sequence[T], amount: Int) extends ProduceSequence[T] {
  def termType = TermType.SAMPLE
}