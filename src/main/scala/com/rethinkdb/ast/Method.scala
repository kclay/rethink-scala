package com.rethinkdb.ast

import com.rethinkdb.{MethodTerm, Term}
import ql2.{Ql2=>p}
import com.rethinkdb.conversions.Tokens._


/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 4/2/13
 * Time: 7:36 PM
 * To change this template use File | Settings | File Templates.
 */

case class Append(array: Array[Any], other: Term) extends MethodTerm {
  override lazy val args = buildArgs(array, other)

  def termType: TokenType = p.Term.TermType.APPEND
}

case class Slice(target: Term, left: Int, right: Int) extends Term {
  override lazy val args = buildArgs(target, left, right)

  def termType: TokenType = p.Term.TermType.SLICE
}

case class Skip(target: Term, amount: Int) extends Term with MethodTerm {
  override lazy val args = buildArgs(target, amount)

  def termType: TokenType = p.Term.TermType.SKIP
}


case class Limit(target: Term, amount: Int) extends Term with MethodTerm {
  override lazy val args = buildArgs(target, amount)

  def termType: TokenType = p.Term.TermType.LIMIT
}

case class GetAttr(target: Term, name: String) extends Term {
  override lazy val args = buildArgs(name)

  def termType = p.Term.TermType.GETATTR
}


case class Contains(target: Term, attribute:Iterable[String]) extends Term {
  override lazy val args = buildArgs(target, attribute)

  def termType: TokenType = p.Term.TermType.CONTAINS
}

case class Pluck(target: Term, attributes: Iterable[String]) extends MethodTerm {
  override lazy val args = buildArgs(target, attributes)

  def termType: TokenType = p.Term.TermType.PLUCK
}

case class Without(target: Term, attributes: Iterable[String]) extends MethodTerm {
  override lazy val args = buildArgs(target, attributes)

  def termType: TokenType = p.Term.TermType.WITHOUT
}

case class Merge(other: Term) extends MethodTerm {
  override lazy val args = buildArgs(other)

  def termType: TokenType = p.Term.TermType.MERGE
}



case class Between(start: Int, end: Int) extends MethodTerm {
  override lazy val args = buildArgs(start, end)

  def termType: TokenType = p.Term.TermType.BETWEEN
}
