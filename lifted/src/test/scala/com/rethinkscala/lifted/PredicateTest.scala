package com.rethinkscala.lifted

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 7/4/13
 * Time: 3:45 PM 
 */
object PredicateTest extends App {

  case class Foo(bar: String)

  println(LiftedPredicate((f: Foo) => f.bar))
}
