package com.rethinkscala.changefeeds.ast

import com.rethinkscala.Document
import com.rethinkscala.ast.{ProduceChangeStream, Table}
import ql2.Ql2.Term.TermType

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 2/1/15
 * Time: 11:15 AM
 *
 */
case class Changes[T<:Document](target:Table[T]) extends ProduceChangeStream[T]{
  override def termType =TermType.CHANGES
}

