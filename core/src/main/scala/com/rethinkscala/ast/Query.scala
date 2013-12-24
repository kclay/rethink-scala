package com.rethinkscala.ast

import com.rethinkscala.Term

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 12/24/13
 * Time: 9:19 AM 
 */
trait Query extends Term {


  val stmt: String = ""

  implicit def term2Query(t: Term): Query = t.asInstanceOf[Query]

  private[this] def print: String = "not implemented"

  //print(this)

  private def print(term: Term): String = {


    term.print(term.args.map(print(_)), term.optargs.map(p => (p.key, print(p.token))).toMap)

  }

  private[rethinkscala] def print(args: Seq[String], opt: Map[String, String]): String = ???
}


trait TopLevelQuery extends Query {

}

trait MethodQuery extends Query {
  /*
   def compose(self, args, optargs):
     if needs_wrap(self.args[0]):
         args[0] = T('r.expr(', args[0], ')')

     restargs = args[1:]
     restargs.extend([T(k, '=', v) for k,v in optargs.items()])
     restargs = T(*restargs, intsp=', ')

     return T(args[0], '.', self.st, '(', restargs, ')')
   */
}


abstract class Math {
  val left: Typed
  val right: Typed
}

trait BiCompareOperQuery extends Math with Query {

}