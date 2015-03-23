package com.rethinkscala.ast



import com.rethinkscala.{BoundOptions, Term}
import ql2.Ql2.Term.TermType

abstract class Transformation[T,C[_],R] extends ProduceSeq[R,C] {
  val target: Sequence[T,C]
  val func: FuncWrap

  override lazy val args = buildArgs(target, func())

}

/** Transform each element of the sequence by applying the given mapping function.
  * @param target
  * @param func
  */
case class RMap[T,C[_],R](target: Sequence[T,C], func: FuncWrap) extends Transformation[T,C,R] {

  def termType:TermType = TermType.MAP

  def toConcat = ConcatMap(target, func)

}

/** Flattens a sequence of arrays returned by the mappingFunction into a single sequence.
  * @param target
  * @param func
  */
case class ConcatMap[T,C[_],R](target: Sequence[T,C], func: FuncWrap) extends Transformation[T,C,R] {

  def termType:TermType = TermType.CONCAT_MAP

  def toMap = RMap(target, func)
}

/** Takes a sequence of objects and a list of fields. If any objects in the sequence don't have all of
  * the specified fields, they're dropped from the sequence. The remaining objects have the specified fields plucked out.
  * (This is identical to `has_fields` followed by `pluck` on a sequence.)
  * @param target
  * @param fields
  */
case class WithFields[T,C[_]](target: Sequence[T,C], fields: Seq[Any]) extends ProduceSeq[T,C] {

  def termType:TermType = TermType.WITH_FIELDS

  override lazy val args = buildArgs(fields.+:(target): _*)
}

case class SliceRange(start: Int = 0, end: Int = -1)


trait Order

abstract class Ordering extends Term with Order {
  val attr: FuncWrap

  def flip: Ordering

  override lazy val args: Seq[Term] = buildArgs(attr)
}

case class Asc(attr: FuncWrap) extends Ordering {

  def flip = Desc(attr)

  def termType:TermType = TermType.ASC
}

case class Desc(attr: FuncWrap) extends Ordering {
  def flip = Asc(attr)

  def termType:TermType = TermType.DESC
}

/** Sort the sequence by document values of the given key(s).
  * order by defaults to ascending ordering. To explicitly specify the ordering, wrap the attribute with either r.asc or r.desc.
  * @param target
  * @param values
  */
case class OrderBy[T,C[_]](target: Sequence[T,C], values: Seq[Order], index: Option[Order] = None)
  extends ProduceSeq[T,C] {


  override lazy val args = buildArgs( values.+:(target): _*)

  override lazy val optargs = buildOptArgs(Map("index"->index))
  def termType:TermType = TermType.ORDER_BY

  def withIndex(i: String):OrderBy[T,C] = copy(index = Some(i))
  def withIndex(i: Order):OrderBy[T,C] = copy(index = Some(i))
}

/** Skip a number of elements from the head of the sequence.
  * @param target
  * @param index
  */
case class Skip[T,C[_]](target: Sequence[T,C], index: Int) extends MethodQuery with ProduceSeq[T,C] {
  def termType:TermType = TermType.SKIP
}

/** Concatenate two sequences.
  * @param target
  * @param others
  */
case class Union[T,C[_],R,CR[_]](target: Sequence[T,C], others: Sequence[R,CR]) extends ProduceAnySequence {

  override lazy val args: Seq[Term] = buildArgs(target, others)

  def termType:TermType = TermType.UNION
}

/** Get the nth element of a sequence.
  * @param target
  * @param left
  * @param right
  */
case class Slice[T,C[_]](target: Sequence[T,C], left: Int = 0, right: Int = -1, bounds: BoundOptions)
  extends MethodQuery with ProduceSeq[T,C] {
  override lazy val args = buildArgs(target, left, right)

  override lazy val optargs = buildOptArgs(bounds.toMap)

  def termType:TermType = TermType.SLICE
}

/** End the sequence after the given number of elements.
  * @param target
  * @param amount
  */
case class Limit[T,C[_]](target: Sequence[T,C], amount: Int) extends MethodQuery with ProduceSeq[T,C] {
  override lazy val args = buildArgs(target, amount)

  def termType:TermType = TermType.LIMIT
}

/** Test if a sequence is empty.
  * @param target
  */
case class IsEmpty[T,C[_]](target: Sequence[T,C]) extends ProduceBinary {
  def termType:TermType = TermType.IS_EMPTY
}

/** Get the indexes of an element in a sequence. If the argument is a predicate, get the indexes of all elements matching it.
  * @param target
  * @param filter
  */
case class IndexesOf[T,C[_]](target: Sequence[T,C], filter: FuncWrap) extends ProduceSeq[Long,C] {

  override lazy val args = buildArgs(target, filter)

  def termType:TermType = TermType.INDEXES_OF
}

case class Nth[T](target: IndexTyped[T], index: Int) extends ProduceSingleSelection[T] {
  def termType:TermType = TermType.NTH
}

/** Select a given number of elements from a sequence with uniform random distribution. Selection is done without replacement.
  * @param target
  * @param amount
  */
case class Sample[T,C[_]](target: Sequence[T,C], amount: Int) extends ProduceSeq[T,C] {
  def termType:TermType = TermType.SAMPLE
}