package com.rethinkscala.ast

import com.rethinkscala.Term

import ql2.Term.TermType

/** Append a value to an array.
 *  @param target
 *  @param value
 */
case class Append(target: ArrayTyped, value: Datum) extends ProduceArray {

  def termType = TermType.APPEND
}

/** Prepend a value to an array.
 *  @param target
 *  @param value
 */
case class Prepend(target: ArrayTyped, value: Datum) extends ProduceArray {
  def termType = TermType.PREPEND
}

/** Get a single attribute from an object.
 *  @param target
 *  @param name
 */
case class GetAttr(target: Json, name: String) extends ProduceAny {
  override lazy val args = buildArgs(target, name)

  def termType = TermType.GETATTR
}

abstract class Pluck extends Term {

  val target: Typed
  val attrs: Seq[String]

  override lazy val args = buildArgs(attrs.+:(target): _*)

  def termType = TermType.PLUCK
}

object Pluck {
  def apply(target: Sequence, attrs: Seq[String]) = SPluck(target, attrs)

  def apply(target: Json, attrs: Seq[String]) = OPluck(target, attrs)
}

/** Plucks out one or more attributes from either an object or a sequence of objects (projection).
 *  @param target
 *  @param attrs
 */
case class SPluck(target: Sequence, attrs: Seq[String]) extends Pluck with ProduceSequence

/** Plucks out one or more attributes from either an object or a sequence of objects (projection).
 *  @param target
 *  @param attrs
 */
case class OPluck(target: Json, attrs: Seq[String]) extends Pluck with ProduceDocument

abstract class Without(target: Typed, attributes: Seq[String]) extends Term {

  override lazy val args = buildArgs(attributes.+:(target): _*)

  def termType = TermType.WITHOUT
}

object Without {

  def apply(target: Sequence, attrs: Seq[String]) = new Without(target, attrs) with ProduceSequence

  def apply(target: Json, attrs: Seq[String]) = new Without(target, attrs) with ProduceDocument
}

/** Merge two objects together to construct a new object with properties from both. Gives preference to attributes from other when there is a conflict.
 *  @param target
 *  @param other
 */
abstract class Merge(target: Typed, other: Typed) extends Term {

  override lazy val args = buildArgs(target, other)

  def termType = TermType.MERGE
}

object Merge {

  def apply(target: Sequence, other: Sequence) = new Merge(target, other) with ProduceSequence

  def apply(target: Json, other: Json) = new Merge(target, other) with ProduceDocument
  def apply(target: Ref, other: Ref) = new Merge(target, other) with ProduceAny
}

/** Remove the elements of one array from another array.
 *  @param target
 *  @param diff
 */
case class Difference(target: ArrayTyped, diff: Seq[Datum]) extends ProduceArray {
  override lazy val args = buildArgs(target, MakeArray(diff))
  def termType = TermType.DIFFERENCE
}

/** Add a value to an array and return it as a set (an array with distinct values).
 *  @param target
 *  @param value
 */
case class SetInsert(target: ArrayTyped, value: Datum) extends ProduceSet {
  override lazy val args = buildArgs(target, value)
  def termType = TermType.SET_INSERT
}

/** Add a several values to an array and return it as a set (an array with distinct values).
 *  @param target
 *  @param values
 */
case class SetUnion(target: ArrayTyped, values: Seq[Datum]) extends ProduceSet {

  override lazy val args = buildArgs(target, MakeArray(values))
  def termType = TermType.SET_UNION
}

/** Intersect two arrays returning values that occur in both of them as a set (an array with distinct values).
 *  @param target
 *  @param values
 */
case class SetIntersection(target: ArrayTyped, values: Seq[Datum]) extends ProduceSet {

  override lazy val args = buildArgs(target, MakeArray(values))
  def termType = TermType.SET_INTERSECTION
}

/** Remove the elements of one array from another and return them as a set (an array with distinct values).
 *  @param target
 *  @param values
 */
case class SetDifference(target: Sequence, values: Seq[Datum]) extends ProduceSet {

  override lazy val args = buildArgs(target, MakeArray(values))
  def termType = TermType.SET_DIFFERENCE
}

/** Test if an object has all of the specified fields. An object has a field if it has the specified
 *  key and that key maps to a non-null value. For instance, the object `{'a':1,'b':2,'c':null}` has the fields `a` and `b`.
 *  @param target
 *  @param fields
 */
case class HasFields(target: Json, fields: Seq[String]) extends ProduceBinary {

  override lazy val args = buildArgs(fields.+:(target): _*)

  def termType = TermType.HAS_FIELDS
}

/** Insert a value in to an array at a given index. Returns the modified array.
 *  @param target
 *  @param index
 *  @param value
 */
case class InsertAt(target: ArrayTyped, index: Int, value: Datum) extends ProduceArray {
  def termType = TermType.INSERT_AT
}

/** Insert several values in to an array at a given index. Returns the modified array.
 *  @param target
 *  @param index
 *  @param values
 */
case class SpliceAt(target: ArrayTyped, index: Int, values: Seq[Datum]) extends ProduceArray {

  override lazy val args = buildArgs(target, index, MakeArray(values))

  def termType = TermType.SPLICE_AT
}

/** Remove an element from an array at a given index. Returns the modified array.
 *  @param target
 *  @param start
 *  @param end
 */

case class DeleteAt(target: ArrayTyped, start: Int, end: Option[Int] = None) extends ProduceArray {

  override lazy val args = buildArgs(end.map(Seq(target, start, _)).getOrElse(Seq(target, start)): _*)

  def termType = TermType.DELETE_AT
}

/** Change a value in an array at a given index. Returns the modified array.
 *  @param target
 *  @param index
 *  @param value
 */
case class ChangeAt(target: ArrayTyped, index: Int, value: Datum) extends ProduceArray {
  def termType = TermType.CHANGE_AT
}

/** Match against a regular expression. Returns a match object containing the matched string,
 *  that string's start/end position, and the capture groups.
 *  Accepts RE2 syntax (https://code.google.com/p/re2/wiki/Syntax). You can enable case-insensitive matching by
 *  prefixing the regular expression with `(?i)`. (See linked RE2 documentation for more flags.)
 *  @param target
 *  @param regexp
 */
case class Match(target: Strings, regexp: String) extends ProduceDocument with Binary {
  def termType = TermType.MATCH
}

/** Gets the type of a value.
 *  @param target
 */
case class TypeOf(target: Typed) extends ProduceString {
  def termType = TermType.TYPEOF
}

case class Keys(target: Json) extends ProduceArray {
  def termType = TermType.KEYS
}

