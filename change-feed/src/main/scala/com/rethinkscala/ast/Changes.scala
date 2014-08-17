package com.rethinkscala.ast

import com.rethinkscala.{CursorChange,  Document}

import ql2.Ql2.Term.TermType
/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 8/16/14
 * Time: 8:19 PM
 *
 */

case class Changes[T<:Document](target:Table[T]) extends ProduceStreamSelection[CursorChange[T]]{
  override def termType =TermType.CHANGES
}