package com.rethinkscala.ast

import com.rethinkscala.net.AbstractCursor
import ql2.Ql2.Term.TermType

import com.rethinkscala.GroupResult

/** Produce a single value from a sequence through repeated application of a reduction function.
  * The reduce function gets invoked repeatedly not only for the input values but also for results of
  * previous reduce invocations. The type and format of the object that is passed in to reduce must be
  * the same with the one returned from reduce.
  * @param target
  * @param f
  *
  */
case class Reduce[T,R](target: Aggregation[T], f: Predicate2) extends Produce[R] {

  override lazy val args = buildArgs(target, f())


  def termType:TermType = TermType.REDUCE
}



/** Count the number of elements in the sequence. With a single argument, count the number of elements equal to it.
  * If the argument is a function, it is equivalent to calling filter before count.
  * @param target
  */
case class Count(target: Aggregation[_], wrap: Option[FuncWrap] = None) extends ProduceNumeric {

  override lazy val args = buildArgs((wrap.map(Seq(target, _)).getOrElse(Seq(target))): _*)

  def termType:TermType = TermType.COUNT
}

/** Remove duplicate elements from the sequence.
  * @param target
  */
case class Distinct[T,C[_]](target: Sequence[T,C]) extends ProduceSeq[T,C] {
  def termType:TermType = TermType.DISTINCT
}


case class Group[R, T](target: Aggregation[T], wrap: Seq[FuncWrap]) extends ProduceGroup[R] {


  override lazy val args = buildArgs(wrap.+:(target): _*)

  def termType:TermType = TermType.GROUP
}


/** Groups elements by the values of the given attributes and then applies the given reduction.
  * Though similar to grouped_map_reduce, groupby takes a standardized object
  * @param target
  * @param method
  * @param attrs
  */
/*
case class GroupBy[T](target: Sequence[T], method: AggregateByMethod, attrs: Seq[String]) extends ProduceSequence[GroupResult] {

  override lazy val args = buildArgs((Seq(target, method.underlying) ++ attrs): _*)

  def termType:TermType = TermType.GROUPBY
}
     */
/** Test if an object has the given attribute.
  * @param target
  * @param value
  */
case class Contains[T](target: Aggregation[T], value: Seq[FuncWrap]) extends MethodQuery with ProduceBinary {
  override lazy val args = buildArgs(value.+:(target): _*)

  def termType:TermType = TermType.CONTAINS
}


trait MethodAggregation[T]  extends MethodQuery with Typed{
  self:Typed=>
  val target:Aggregation[T]
  val fieldOrFunction:Option[FuncWrap]

  override lazy val args = buildArgs(Seq(Some(target),fieldOrFunction).flatten:_*)
}

case class Max[T](target:Aggregation[T],fieldOrFunction:Option[FuncWrap]=None) extends  MethodAggregation[T] with ProduceSingle[T]{

  def termType:TermType = TermType.MAX
}

case class Min[T](target:Aggregation[T],fieldOrFunction:Option[FuncWrap]=None) extends MethodAggregation[T] with ProduceSingle[T]{

  def termType:TermType = TermType.MIN
}
case class Sum[T](target:Aggregation[T],fieldOrFunction:Option[FuncWrap] = None) extends MethodAggregation[T] with ProduceNumeric{

  def termType:TermType = TermType.SUM
}

case class Avg[T](target:Aggregation[T],fieldOrFunction:Option[FuncWrap] = None) extends MethodAggregation[T] with ProduceNumeric{

  def termType:TermType = TermType.AVG
}