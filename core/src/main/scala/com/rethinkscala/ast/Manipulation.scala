package com.rethinkscala.ast

import com.rethinkscala.changefeeds.net.RethinkChangesIterator
import com.rethinkscala.{Document, Term, MatchResult}
import com.rethinkscala.net.{RethinkCursor, DefaultCursor}
import ql2.Ql2.Term.TermType

/** Append a value to an array.
  * @param target
  * @param value
  */
case class Append[T](target: ArrayTyped[T], value: T) extends MethodQuery with ProduceArray[T] {

  def termType: TermType = TermType.APPEND
}

/** Prepend a value to an array.
  * @param target
  * @param value
  */
case class Prepend[T, R >: T](target: ArrayTyped[T], value: R) extends MethodQuery with ProduceArray[T] {
  def termType: TermType = TermType.PREPEND
}

/** Get a single attribute from an object.
  * @param target
  * @param name
  */
abstract class GetField(target: Typed, name: String) extends Term {

  override lazy val args = buildArgs(target, name)

  def termType: TermType = TermType.GET_FIELD

}


object GetField {

  def apply[T, C[_]](target: Sequence[T, C], name: String): ProduceArray[T] = new GetField(target, name) with ProduceArray[T]

  def apply(target: Typed, name: String): ProduceAny = new GetField(target, name) with ProduceAny

  def any(target: Typed, name: String): ProduceAny = new GetField(target, name) with ProduceAny
}


abstract class Pluck extends Term {

  val target: Typed

  val data: Either[Seq[String], Map[String, Any]]

  override lazy val args = buildArgs(target, data match {
    case Left(a) => a
    case Right(b) => b
  })

  def termType: TermType = TermType.PLUCK
}

object Pluck {

  def apply[T, C[_]](target: Sequence[T, C], attrs: Seq[String]) = SPluck(target, Left(attrs))

  def apply[T, C[_]](target: Sequence[T, C], m: Map[String, Any]) = SPluck(target, Right(m))

  def apply(target: Record, attrs: Seq[String]) = OPluck(target, Left(attrs))

  def apply(target: Record, m: Map[String, Any]) = OPluck(target, Right(m))
}


/** Plucks out one or more attributes from either an object or a sequence of objects (projection).
  * @param target
  * @param data
  */
case class SPluck[T, C[_]](target: Sequence[T, C], data: Either[Seq[String], Map[String, Any]]) extends Pluck
with ProduceSeq[Map[String, Any], C]

/** Plucks out one or more attributes from either an object or a sequence of objects (projection).
  * @param target
  * @param data
  */
case class OPluck[T](target: Record, data: Either[Seq[String], Map[String, Any]]) extends Pluck
with ProduceSingle[T]

abstract class Without(target: Typed, attributes: Seq[String]) extends Term {

  override lazy val args = buildArgs(attributes.+:(target): _*)

  def termType: TermType = TermType.WITHOUT
}

object Without {
  // FIXME
  def apply[T, C[_]](target: Sequence[T, C], attrs: Seq[String]) = new Without(target, attrs) with ProduceSeq[Map[String, Any], C]

  def apply(target: Record, attrs: Seq[String]) = new Without(target, attrs) with ProduceAnyDocument
}

/** Merge two objects together to construct a new object with properties from both. Gives preference to attributes from other when there is a conflict.
  * @param target
  * @param other
  */
abstract class Merge(target: Typed, other: Typed) extends Term {

  override lazy val args = buildArgs(target, other)

  def termType: TermType = TermType.MERGE

}

case class MergeSequence[T, R >: T, C[_], CR[_]](left: Sequence[T, C], right: Sequence[R, CR]) extends Merge(left, right)
with ProduceSeq[T, C]

object Merge {

  def seq[T, C[_], R, CR[_]](target: Sequence[T, C], other: Sequence[R, CR]) = new Merge(target, other) with ProduceAnySequence

  def apply[T, C[_]](target: Sequence[T, C], other: MakeObj) = new Merge(target, other) with ProduceAnySequence

  def selection[T, R](target: Selection[T], other: Selection[R]) = new Merge(target, other) with ProduceSingleSelection[Any]


  def record(target: Record, other: Any): ProduceAnyDocument = new Merge(target, Expr(other).asInstanceOf[Typed]) with ProduceAnyDocument

  def typed(target: Typed, other: Typed): ProduceAnyDocument = new Merge(target, other) with ProduceAnyDocument

}

/** Remove the elements of one array from another array.
  * @param target
  * @param array
  */
case class Difference[T, R](target: ArrayTyped[T], array: ArrayTyped[R]) extends
MethodQuery with ProduceArray[T] {
  override lazy val args = buildArgs(target, array)

  def termType: TermType = TermType.DIFFERENCE
}

/** Add a value to an array and return it as a set (an array with distinct values).
  * @param target
  * @param value
  */
case class SetInsert[T, R >: T](target: ArrayTyped[T], value: R) extends MethodQuery with ProduceSet[T] {
  override lazy val args = buildArgs(target, value)

  def termType: TermType = TermType.SET_INSERT
}

/** Add a several values to an array and return it as a set (an array with distinct values).
  * @param target
  * @param value
  */
case class SetUnion[T, R >: T](target: ArrayTyped[T], value: ArrayTyped[R]) extends MethodQuery with ProduceSet[T] {

  override lazy val args = buildArgs(target, value)

  def termType: TermType = TermType.SET_UNION
}

/** Intersect two arrays returning values that occur in both of them as a set (an array with distinct values).
  * @param target
  * @param values
  */
case class SetIntersection[T, R >: T](target: ArrayTyped[T], values: ArrayTyped[R]) extends MethodQuery with ProduceSet[T] {

  override lazy val args = buildArgs(target, values)

  def termType: TermType = TermType.SET_INTERSECTION
}

/** Remove the elements of one array from another and return them as a set (an array with distinct values).
  * @param target
  * @param values
  */
case class SetDifference[T, R >: T](target: ArrayTyped[T], values: ArrayTyped[R]) extends MethodQuery with ProduceSet[T] {

  override lazy val args = buildArgs(target, values)

  def termType: TermType = TermType.SET_DIFFERENCE
}

/** Test if an object has all of the specified fields. An object has a field if it has the specified
  * key and that key maps to a non-null value. For instance, the object `{'a':1,'b':2,'c':null}` has the fields `a` and `b`.
  * @param target
  * @param fields
  */
case class HasFields(target: Record, fields: Seq[String]) extends ProduceBinary {

  override lazy val args = buildArgs(fields.+:(target): _*)

  def termType: TermType = TermType.HAS_FIELDS
}

/** Insert a value in to an array at a given index. Returns the modified array.
  * @param target
  * @param index
  * @param value
  */
case class InsertAt[T, R >: T](target: ArrayTyped[T], index: Int, value: R) extends ProduceArray[T] {
  def termType: TermType = TermType.INSERT_AT
}

/** Insert several values in to an array at a given index. Returns the modified array.
  * @param target
  * @param index
  * @param values
  */
case class SpliceAt[T](target: ArrayTyped[T], index: Int, values: Seq[T]) extends ProduceArray[T] {

  override lazy val args = buildArgs(target, index, MakeArray(values))

  def termType: TermType = TermType.SPLICE_AT
}

/** Remove an element from an array at a given index. Returns the modified array.
  * @param target
  * @param start
  * @param end
  */

case class DeleteAt[T](target: ArrayTyped[T], start: Int, end: Option[Int] = None) extends ProduceArray[T] {

  override lazy val args = buildArgs(end.map(Seq(target, start, _)).getOrElse(Seq(target, start)): _*)

  def termType: TermType = TermType.DELETE_AT
}

/** Change a value in an array at a given index. Returns the modified array.
  * @param target
  * @param index
  * @param value
  */
case class ChangeAt[T, B >: T](target: ArrayTyped[T], index: Int, value: B) extends ProduceArray[T] {
  def termType: TermType = TermType.CHANGE_AT
}

/** Match against a regular expression. Returns a match object containing the matched string,
  * that string's start/end position, and the capture groups.
  * Accepts RE2 syntax (https://code.google.com/p/re2/wiki/Syntax). You can enable case-insensitive matching by
  * prefixing the regular expression with `(?i)`. (See linked RE2 documentation for more flags.)
  * @param target
  * @param regexp
  */
case class Match(target: Strings, regexp: String) extends ProduceDocument[MatchResult] {
  def termType: TermType = TermType.MATCH
}

/** Gets the type of a value.
  * @param target
  */
case class TypeOf(target: Typed) extends ProduceString {
  def termType: TermType = TermType.TYPE_OF
}

case class Keys(target: Record) extends ProduceArray[String] {
  def termType: TermType = TermType.KEYS
}

// FIXME support change cursor split
case class Split(target: Strings, delimiter: Option[String] = None, limit: Option[Int] = None) extends ProduceSeq[String, RethinkCursor] {

  override lazy val args = buildArgs(Seq(Some(target), delimiter, limit).flatten: _*)

  def termType: TermType = TermType.SPLIT
}
