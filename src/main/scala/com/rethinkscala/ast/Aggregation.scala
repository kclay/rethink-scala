package com.rethinkscala.ast

import ql2.Term.TermType

/** Produce a single value from a sequence through repeated application of a reduction function.
 *  The reduce function gets invoked repeatedly not only for the input values but also for results of
 *  previous reduce invocations. The type and format of the object that is passed in to reduce must be
 *  the same with the one returned from reduce.
 *  @param target
 *  @param f
 *  @param base
 */
case class Reduce(target: Sequence, f: Predicate2, base: Option[Any] = None) extends ProduceAny {

  override lazy val args = buildArgs(target, f())

  override lazy val optargs = buildOptArgs(Map("base" -> base))

  def termType = TermType.REDUCE
}

/** Count the number of elements in the sequence. With a single argument, count the number of elements equal to it.
 *  If the argument is a function, it is equivalent to calling filter before count.
 *  @param target
 *  @param filter
 */
case class Count(target: Sequence, filter: Option[Either[String, BooleanPredicate]] = None) extends ProduceNumeric {

  override lazy val args = buildArgs(filter.map {
    a =>
      Seq(target, a match {

        case Left(x)  => x
        case Right(f) => f()
      })
  }.getOrElse(Seq(target)): _*)

  def termType = TermType.COUNT
}

/** Remove duplicate elements from the sequence.
 *  @param target
 */
case class Distinct(target: Sequence) extends ProduceSequence {
  def termType = TermType.DISTINCT
}

/** Partition the sequence into groups based on the grouping function. The elements of each group are then mapped
 *  using the mapping function and reduced using the reduction function.
 *  @param target
 *  @param grouping
 *  @param mapping
 *  @param reduce
 *  @param base
 */
case class GroupMapReduce(target: Sequence, grouping: Predicate1, mapping: Predicate1, reduce: Predicate2, base: Option[Datum] = None) extends ProduceArray {

  override lazy val args = buildArgs(target, grouping(), mapping(), reduce())
  override lazy val optargs = buildOptArgs(Map("base" -> base))

  def termType = TermType.GROUPED_MAP_REDUCE
}

/** Groups elements by the values of the given attributes and then applies the given reduction.
 *  Though similar to grouped_map_reduce, groupby takes a standardized object
 *  @param target
 *  @param method
 *  @param attrs
 */
case class GroupBy(target: Sequence, method: AggregateByMethod, attrs: Seq[String]) extends ProduceSequence {

  override lazy val args = buildArgs((Seq(target, method.underlying) ++ attrs): _*)

  def termType = TermType.GROUPBY
}

/** Test if an object has the given attribute.
 *  @param target
 *  @param value
 */
case class Contains(target: Sequence, value: Seq[Datum]) extends ProduceBinary {
  override lazy val args = buildArgs((Seq(target) ++ value): _*)

  def termType = TermType.CONTAINS
}
