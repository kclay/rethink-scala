package com.rethinkdb.ast

import com.rethinkdb.Term
import ql2.{Ql2=>p}
import com.rethinkdb.conversions.Tokens._

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 4/2/13
 * Time: 7:41 PM
 * To change this template use File | Settings | File Templates.
 */

case class Get(from: Term, attribute: String) extends Term {
  override lazy val args = buildArgs(from, attribute)

  def termType: TokenType = p.Term.TermType.GET
}
class Functional {


  // TODO : Reduce

  // TODO : Map
  // TODO : Filter
  // TODO : ConcatMap
  // TODO : OrderBy
  // TODO : Distinct
  // TODO : Count
  // TODO : Union
  // TODO : Nth
  // TODO : GroupedMapReduce
  // TODO : GroupBy
  // TODO : InnerJoin
  // TODO : OuterJoin
  // TODO : EqJoin
  // TODO : Zip
  // TODO : CoerceTo
  // TODO : TypeOf
  // TODO : Update  ^
  // TODO : Delete
  // TODO : Replace
  // TODO : Insert





}
