package com.rethinkdb

import ql2.Ql2._

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 3/19/13
 * Time: 12:06 PM 
 */
abstract class BaseQuery(args: Seq[AnyRef], options: Option[Map[String, AnyRef]]) {


  val term: Term.TermType


}

class T(args: Seq[AnyRef], inspect: String = "") extends Iterator[AnyRef] {
  var last = "0"

  // mutable state
  def hasNext = true // the twitter stream has no end

  def next() = {
    for (a <- args) yield a
  }
}


class Var(args: Seq[AnyRef], options: Option[Map[String, AnyRef]]) extends BaseQuery(args, options) {
  val term = Term.TermType.VAR
}


