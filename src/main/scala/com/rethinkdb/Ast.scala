package com.rethinkdb

import ql2.Ql2._
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 3/19/13
 * Time: 12:06 PM 
 */
trait BaseQuery {


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


sealed trait ExprWrap;


case class DataNum(data:AnyRef)

case class Var(name: String) {
  val term = Term.TermType.VAR
}

class JavaScript extends BaseQuery {
  val term = Term.TermType.JAVASCRIPT
}



case class Eq extends BaseQuery {
  val term = Term.TermType.EQ
}

class Ne extends BaseQuery {
  val term = Term.TermType.NE
}

class Lt extends BaseQuery {
  val term = Term.TermType.LT
}

class Le extends BaseQuery {
  val term = Term.TermType.LE
}

