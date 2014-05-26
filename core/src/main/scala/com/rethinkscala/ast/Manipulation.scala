package com.rethinkscala.ast

import com.rethinkscala.Term
import ql2.Ql2.Term.TermType
import com.rethinkscala.MatchResult

/** Append a value to an array.
  * @param target
  * @param value
  */
case class Append[T](target: ArrayTyped[T], value: T) extends MethodQuery with ProduceArray[T] {

  def termType = TermType.APPEND
}

/** Prepend a value to an array.
  * @param target
  * @param value
  */
case class Prepend[T,R >:T](target: ArrayTyped[T], value: R) extends MethodQuery with ProduceArray[T] {
  def termType = TermType.PREPEND
}

/** Get a single attribute from an object.
  * @param target
  * @param name
  */
abstract class GetField(target: Typed, name: String) extends Term {


  override lazy val args = buildArgs(target, name)

  def termType = TermType.GET_FIELD



}


object GetField {


  //def apply(target: ArrayTyped, name: String) = new GetField(target, name) with ProduceArray


  def apply[T](target: Sequence[_], name: String) = new GetField(target, name) with ProduceArray[T]

  // def apply(target: Record, name: String) = new GetField(target, name) with ProduceAnyDocument

  def apply(target: Typed, name: String) = new GetField(target, name) with ProduceAny
}


abstract class Pluck extends Term {

  val target: Typed

  val data: Either[Seq[String], Map[String, Any]]


  override lazy val args = buildArgs(target, data match {
    case Left(a) => a
    case Right(b) => b
  })

  def termType = TermType.PLUCK
}

object Pluck {


  def apply[T](target: Sequence[T], attrs: Seq[String]) = SPluck(target, Left(attrs))

  def apply[T](target: Sequence[T], m: Map[String, Any]) = SPluck(target, Right(m))


  def apply(target: Record, attrs: Seq[String]) = OPluck(target, Left(attrs))

  def apply(target: Record, m: Map[String, Any]) = OPluck(target, Right(m))
}


/** Plucks out one or more attributes from either an object or a sequence of objects (projection).
  * @param target
  * @param data
  */
case class SPluck[T](target: Sequence[T], data: Either[Seq[String], Map[String, Any]]) extends Pluck
with ProduceSequence[Map[String,Any]]

/** Plucks out one or more attributes from either an object or a sequence of objects (projection).
  * @param target
  * @param data
  */
case class OPluck[T](target: Record, data: Either[Seq[String], Map[String, Any]]) extends Pluck
with ProduceSingle[T]

abstract class Without(target: Typed, attributes: Seq[String]) extends Term {

  override lazy val args = buildArgs(attributes.+:(target): _*)

  def termType = TermType.WITHOUT
}

object Without {

  def apply(target: Sequence[_], attrs: Seq[String]) = new Without(target, attrs) with ProduceSequence[Map[String,Any]]

  def apply(target: Record, attrs: Seq[String]) = new Without(target, attrs) with ProduceAnyDocument
}

/** Merge two objects together to construct a new object with properties from both. Gives preference to attributes from other when there is a conflict.
  * @param target
  * @param other
  */
abstract class Merge(target: Typed, other: Typed) extends Term {

  override lazy val args = buildArgs(target, other)

  def termType = TermType.MERGE
}

//abstract class MergeKind[A[_]:  Produce0[_],T,R>:T](left: A[T],right:A[R])  extends Merge(left,right)


/*
trait Merge
trait MergeType[T,Ops<:MergeOp]{


  def merge[T,R>:T](left:Ops#Ast[T],right:Ops#Ast[R])


}
trait MergeOp{
  type Ast[B]

  type Result[_,T,R>:T] <:Merge
  def apply[T,R>:T](left:Ast[T],right:Ast[R]):Result
}
object MergeSequence extends MergeOp{
  type Ast[B] = Sequence[B]


  type Result[_,T,R>:T] = MergeKind[Sequence[_],T,R]
  def apply[T,R>:T](left:Ast[T],right:Ast[R]):Result =  MergeSequence(left,right)
} */
case class MergeSequence[T,R>:T](left:Sequence[T],right:Sequence[R]) extends Merge(left,right)
   with ProduceSequence[T]



object Merge {

  def apply(target: Sequence[_], other: Sequence[_]) = new Merge(target, other) with ProduceAnySequence

  def apply(target: Sequence[_], other: MakeObj) = new Merge(target, other) with ProduceAnySequence

  def apply(target: Record, other: Map[String, Any]) = new Merge(target, Expr(other)) with ProduceAnyDocument

  def apply[P1<:Pluck,M1<:Merge,W1<:Without,P2<:Pluck,M2<:Merge,W2<:Without](target: CanManipulate[P1,M1,W1], other: CanManipulate[P1,M1,W1]) = new Merge(target, other) with ProduceAnyDocument

 // def apply(target: Ref, other: Ref) = new Merge(target, other) with ProduceAny
}

/** Remove the elements of one array from another array.
  * @param target
  * @param array
  */
case class Difference[T,R](target: ArrayTyped[T], array: ArrayTyped[R]) extends
  MethodQuery with ProduceArray[T] {
  override lazy val args = buildArgs(target, array)

  def termType = TermType.DIFFERENCE
}

/** Add a value to an array and return it as a set (an array with distinct values).
  * @param target
  * @param value
  */
case class SetInsert[T,R>:T](target: ArrayTyped[T], value: R) extends MethodQuery with ProduceSet[T] {
  override lazy val args = buildArgs(target, value)

  def termType = TermType.SET_INSERT
}

/** Add a several values to an array and return it as a set (an array with distinct values).
  * @param target
  * @param value
  */
case class SetUnion[T,R>:T](target: ArrayTyped[T], value: ArrayTyped[R]) extends MethodQuery with ProduceSet[T] {

  override lazy val args = buildArgs(target, value)

  def termType = TermType.SET_UNION
}

/** Intersect two arrays returning values that occur in both of them as a set (an array with distinct values).
  * @param target
  * @param values
  */
case class SetIntersection[T,R>:T](target: ArrayTyped[T], values: ArrayTyped[R]) extends MethodQuery with ProduceSet[T] {

  override lazy val args = buildArgs(target, values)

  def termType = TermType.SET_INTERSECTION
}

/** Remove the elements of one array from another and return them as a set (an array with distinct values).
  * @param target
  * @param values
  */
case class SetDifference[T,R>:T](target: ArrayTyped[T], values: ArrayTyped[R]) extends MethodQuery with ProduceSet[T] {

  override lazy val args = buildArgs(target, values)

  def termType = TermType.SET_DIFFERENCE
}

/** Test if an object has all of the specified fields. An object has a field if it has the specified
  * key and that key maps to a non-null value. For instance, the object `{'a':1,'b':2,'c':null}` has the fields `a` and `b`.
  * @param target
  * @param fields
  */
case class HasFields(target: Record, fields: Seq[String]) extends ProduceBinary {

  override lazy val args = buildArgs(fields.+:(target): _*)

  def termType = TermType.HAS_FIELDS
}

/** Insert a value in to an array at a given index. Returns the modified array.
  * @param target
  * @param index
  * @param value
  */
case class InsertAt[T,R>:T](target: ArrayTyped[T], index: Int, value: R) extends ProduceArray[T] {
  def termType = TermType.INSERT_AT
}

/** Insert several values in to an array at a given index. Returns the modified array.
  * @param target
  * @param index
  * @param values
  */
case class SpliceAt[T](target: ArrayTyped[T], index: Int, values: Seq[T]) extends ProduceArray[T] {

  override lazy val args = buildArgs(target, index, MakeArray(values))

  def termType = TermType.SPLICE_AT
}

/** Remove an element from an array at a given index. Returns the modified array.
  * @param target
  * @param start
  * @param end
  */

case class DeleteAt[T](target: ArrayTyped[T], start: Int, end: Option[Int] = None) extends ProduceArray[T] {

  override lazy val args = buildArgs(end.map(Seq(target, start, _)).getOrElse(Seq(target, start)): _*)

  def termType = TermType.DELETE_AT
}

/** Change a value in an array at a given index. Returns the modified array.
  * @param target
  * @param index
  * @param value
  */
case class ChangeAt[T,B>:T](target: ArrayTyped[T], index: Int, value: B) extends ProduceArray[T] {
  def termType = TermType.CHANGE_AT
}

/** Match against a regular expression. Returns a match object containing the matched string,
  * that string's start/end position, and the capture groups.
  * Accepts RE2 syntax (https://code.google.com/p/re2/wiki/Syntax). You can enable case-insensitive matching by
  * prefixing the regular expression with `(?i)`. (See linked RE2 documentation for more flags.)
  * @param target
  * @param regexp
  */
case class Match(target: Strings, regexp: String) extends ProduceDocument[MatchResult] {
  def termType = TermType.MATCH
}

/** Gets the type of a value.
  * @param target
  */
case class TypeOf(target: Typed) extends ProduceString {
  def termType = TermType.TYPEOF
}

case class Keys(target: Record) extends ProduceArray[String] {
  def termType = TermType.KEYS
}

