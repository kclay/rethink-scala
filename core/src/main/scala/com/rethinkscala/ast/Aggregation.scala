package com.rethinkscala.ast

import ql2.Ql2.Term.TermType

import com.rethinkscala.{GroupMapReduceResult, DatumOrFunction}

/** Produce a single value from a sequence through repeated application of a reduction function.
  * The reduce function gets invoked repeatedly not only for the input values but also for results of
  * previous reduce invocations. The type and format of the object that is passed in to reduce must be
  * the same with the one returned from reduce.
  * @param target
  * @param f
  * @param base
  */
case class Reduce[T](target: Sequence[T], f: Predicate2, base: Option[Any] = None) extends Produce[T] {

  override lazy val args = buildArgs(target, f())

  override lazy val optargs = buildOptArgs(Map("base" -> base))

  def termType = TermType.REDUCE
}

/** Count the number of elements in the sequence. With a single argument, count the number of elements equal to it.
  * If the argument is a function, it is equivalent to calling filter before count.
  * @param target
  * @param filter
  */
case class Count(target: Sequence[_], wrap:Option[FuncWrap]=None) extends ProduceNumeric {

  override lazy val args = buildArgs((wrap.map(Seq(target,_)).getOrElse(Seq(target))):_*)

  def termType = TermType.COUNT
}

/** Remove duplicate elements from the sequence.
  * @param target
  */
case class Distinct[T](target: Sequence[T]) extends ProduceSequence[T] {
  def termType = TermType.DISTINCT
}

/** Partition the sequence into groups based on the grouping function. The elements of each group are then mapped
  * using the mapping function and reduced using the reduction function.
  * @param target
  * @param grouping
  * @param mapping
  * @param reduce
  * @param base
  */
case class GroupMapReduce[T](target: Sequence[T], grouping: Predicate1, mapping: Predicate1, reduce: Predicate2,
                             base: Option[Typed]) extends ProduceArray[GroupMapReduceResult] {

  override lazy val args = buildArgs(target, grouping, mapping, reduce)
  override lazy val optargs = buildOptArgs(Map("base" -> base))

  def termType = TermType.GROUPED_MAP_REDUCE
}

/** Groups elements by the values of the given attributes and then applies the given reduction.
  * Though similar to grouped_map_reduce, groupby takes a standardized object
  * @param target
  * @param method
  * @param attrs
  */
case class GroupBy[T](target: Sequence[T], method: AggregateByMethod, attrs: Seq[String]) extends ProduceSequence[T] {

  override lazy val args = buildArgs((Seq(target, method.underlying) ++ attrs): _*)

  def termType = TermType.GROUPBY
}

/** Test if an object has the given attribute.
  * @param target
  * @param value
  */
case class Contains(target: Sequence[_], value: Seq[DatumOrFunction]) extends ProduceBinary {
  override lazy val args = buildArgs((Seq(target) ++ value.map(FuncWrap(_))): _*)

  def termType = TermType.CONTAINS
}
