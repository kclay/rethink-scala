package com.rethinkdb.ast

import com.rethinkdb.Term

import ql2.Term.TermType


/**
 * Append a value to an array.
 * @param target
 * @param value
 */
case class Append(target: WithDocument, value: Any) extends ProduceDocument  {
  override lazy val args = buildArgs(target, value)

  def termType = TermType.APPEND
}

/**
 * Get a single attribute from an object.
 * @param target
 * @param name
 */
case class GetAttr(target: Document, name: String) extends ProduceAny  with WithAny{
  override lazy val args = buildArgs(target, name)

  def termType = TermType.GETATTR
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
case class Pluck(target:Sequence, attributes: Iterable[String]) extends ProduceSequence {
  override lazy val args = buildArgs(target, attributes)

  def termType = TermType.PLUCK
}

/**
 * The opposite of pluck; takes an object or a sequence of objects, and removes all attributes except for the ones specified.
 * @param target
 * @param attributes
 */
case class Without(target:Sequence, attributes: Iterable[String]) extends ProduceSequence {
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
