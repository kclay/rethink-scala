package com.rethinkdb.ast

import com.rethinkdb.{ProducesBoolean, Term, TermMessage, MethodTerm}

import ql2.Term.TermType


/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 4/2/13
 * Time: 7:36 PM
 * To change this template use File | Settings | File Templates.
 */

case class Append(target:WithJson,value:Any) extends ProduceJson {
  override lazy val args = buildArgs(target,value)

  def termType = TermType.APPEND
}





case class GetAttr(target: Term, name: String) extends ProduceAny {
  override lazy val args = buildArgs(target, name)

  def termType = TermType.GETATTR
}


case class Contains(target: Term, attribute: Iterable[String]) extends ProduceCondition{
  override lazy val args = buildArgs(target, attribute)

  def termType = TermType.CONTAINS
}

case class Pluck(target: Term, attributes: Iterable[String]) extends MethodTerm {
  override lazy val args = buildArgs(target, attributes)

  def termType = TermType.PLUCK
}

case class Without(target: Term, attributes: Iterable[String]) extends MethodTerm {
  override lazy val args = buildArgs(target, attributes)

  def termType = TermType.WITHOUT
}

case class Merge(target:Term,other: Term) extends ProduceSequence {
  override lazy val args = buildArgs(target,other)

  def termType = TermType.MERGE
}


case class Between(start: Int, end: Int) extends ProduceSequence {
  override lazy val args = buildArgs(start, end)

  def termType = TermType.BETWEEN
}
