package com.rethinkscala.changefeeds.ast

import com.rethinkscala.ast.{ProduceChangeStream, Typed}
import ql2.Ql2.Term.TermType

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 2/1/15
 * Time: 11:15 AM
 *
 */


case class Changes[T](target: Typed, squash: Option[Boolean] = None,
                      includeStates: Option[Boolean] = None) extends ProduceChangeStream[T] {


  override lazy val optargs = buildOptArgs(Map("include_states" -> includeStates, "squash" -> squash))

  def withStates = copy(includeStates = Option(true))


  override def termType = TermType.CHANGES
}





