package com.rethinkdb.ast

import com.rethinkdb.Term

import ql2.Term.TermType
import ql2.Term.TermType.EnumVal


/**
 * Append a value to an array.
 * @param target
 * @param value
 */
case class Append(target: Appendable, value: Any) extends ProduceDocument  {
  override lazy val args = buildArgs(target, value)

  def termType = TermType.APPEND
}
case class Prepend(target:Appendable,value:Typed) extends ProduceSequence{
  def termType = TermType.PREPEND
}

/**
 * Get a single attribute from an object.
 * @param target
 * @param name
 */
case class GetAttr(target: Document, name: String) extends ProduceAny{
  override lazy val args = buildArgs(target, name)

  def termType = TermType.GETATTR
}


case class Nth(target:Sequence,index:Int) extends ProduceAny{
  def termType = TermType.NTH
}

/**
 * Get the indexes of an element in a sequence. If the argument is a predicate, get the indexes of all elements matching it.
 * @param target
 * @param filter
 */
case class IndexesOf(target:Sequence,filter:Either[Datum,BooleanPredicate]) extends ProduceSequence{

  override lazy val args = buildArgs(filter.fold(Seq(target,_),Seq(target,_)):_*)
  def termType = TermType.INDEXES_OF
}

/**
 * Test if a sequence is empty.
 * @param target
 */
case class IsEmpty(target:Sequence) extends ProduceBinary{
  def termType = TermType.IS_EMPTY
}

/**
 * Count the number of elements in the sequence. With a single argument, count the number of elements equal to it.
 * If the argument is a function, it is equivalent to calling filter before count.
 * @param target
 * @param filter
 */
case class Count(target:Countable,filter:Some[BooleanPredicate]=None) extends ProduceNumeric{

  override lazy val args = buildArgs(filter.map(Seq(target,_)) .getOrElse(Seq(target)): _*)
  def termType = TermType.COUNT
}

case class Sample(target:Sequence,amount:Int) extends ProduceSequence{
  def termType = TermType.SAMPLE
}

/**
 * Test if an object has the given attribute.
 * @param target
 * @param attribute
 */
case class Contains(target: Document, attribute: String) extends ProduceBinary  {
  override lazy val args = buildArgs(target, attribute)

  def termType = TermType.CONTAINS
}

/**
 * Plucks out one or more attributes from either an object or a sequence of objects (projection).
 * @param target
 * @param attributes
 */
case class Pluck(target:Document, attributes: Iterable[String]) extends ProduceSequence {
  override lazy val args = buildArgs(target, attributes)

  def termType = TermType.PLUCK
}

/**
 * The opposite of pluck; takes an object or a sequence of objects, and removes all attributes except for the ones specified.
 * @param target
 * @param attributes
 */
case class Without(target:Document, attributes: Iterable[String]) extends ProduceSequence {
  override lazy val args = buildArgs(target, attributes)

  def termType = TermType.WITHOUT
}

/**
 * Merge two objects together to construct a new object with properties from both. Gives preference to attributes from other when there is a conflict.
 * @param target
 * @param other
 */
case class Merge(target: Document, other: Document) extends ProduceDocument{
  override lazy val args = buildArgs(target, other)

  def termType = TermType.MERGE
}


case class Between(target:Sequence,start: Int, end: Int) extends ProduceSequence {
  override lazy val args = buildArgs(start, end)

  def termType = TermType.BETWEEN
}

/**
 * Remove the elements of one array from another array.
 * @param target
 * @param diff
 */
case class Difference(target:Document,diff:Iterable[Any]) extends ProduceDocument{
  override lazy val args=buildArgs(target,diff)
  def termType = TermType.DIFFERENCE
}

/**
 * Add a value to an array and return it as a set (an array with distinct values).
 * @param target
 * @param value
 */
case class SetInsert(target:Sequence,value:Any) extends ProduceSet{
  override lazy val args = buildArgs(target,value)
  def termType = TermType.SET_INSERT
}

/**
 * Add a several values to an array and return it as a set (an array with distinct values).
 * @param target
 * @param values
 */
case class SetUnion(target:Sequence,values:Iterable[Any]) extends ProduceSet{

  override lazy val  args = buildArgs(target,values)
  def termType = TermType.SET_UNION
}

/**
 * Intersect two arrays returning values that occur in both of them as a set (an array with distinct values).
 * @param target
 * @param values
 */
case class SetIntersection(target:Sequence,values:Iterable[Any]) extends ProduceSet{

  override lazy val  args = buildArgs(target,values)
  def termType = TermType.SET_INTERSECTION
}

/**
 * Remove the elements of one array from another and return them as a set (an array with distinct values).
 * @param target
 * @param values
 */
case class SetDifference(target:Sequence,values:Iterable[Any]) extends ProduceSet{

  override lazy val  args = buildArgs(target,values)
  def termType = TermType.SET_DIFFERENCE
}

/**
 * Test if an object has all of the specified fields. An object has a field if it has the specified
 * key and that key maps to a non-null value. For instance, the object `{'a':1,'b':2,'c':null}` has the fields `a` and `b`.
 * @param target
 * @param fields
 */
case class HasFields(target:Document,fields:Iterable[String]) extends ProduceBinary{

  override lazy val args = buildArgs(target,fields)

  def termType = TermType.HAS_FIELDS
}

/**
 * Insert a value in to an array at a given index. Returns the modified array.
 * @param target
 * @param index
 * @param value
 */
case class InsertAt(target:Sequence,index:Int,value:Any) extends ProduceSequence{
  def termType = TermType.INSERT_AT
}

/**
 * Insert several values in to an array at a given index. Returns the modified array.
 * @param target
 * @param index
 * @param values
 */
case class SpliceAt(target:Sequence,index:Int,values:Iterable[String]) extends ProduceSequence{

  def termType = TermType.SPLICE_AT
}

/**
 * Remove an element from an array at a given index. Returns the modified array.
 * @param target
 * @param start
 */
// TODO : implement endindex
case class DeleteAt(target:Sequence,start:Int) extends ProduceSequence{
  def termType = TermType.DELETE_AT
}
case class ChangeAt(target:Sequence,index:Int,value:Any) extends ProduceSequence{
  def termType= TermType.CHANGE_AT
}

/**
 * Match against a regular expression. Returns a match object containing the matched string,
 * that string's start/end position, and the capture groups.
 * Accepts RE2 syntax (https://code.google.com/p/re2/wiki/Syntax). You can enable case-insensitive matching by
 * prefixing the regular expression with `(?i)`. (See linked RE2 documentation for more flags.)
 * @param target
 * @param regexp
 */
case class Match(target:Strings,regexp:String) extends ProduceDocument with Binary{
  def termType= TermType.MATCH
}



