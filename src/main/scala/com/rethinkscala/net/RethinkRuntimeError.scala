package com.rethinkscala.net

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 7/3/13
 * Time: 5:21 PM
 *
 */
//abstract class RethinkError(message:String,term:Term,frames:Iterable[Frame]) extends Exception(message)
case class RethinkRuntimeError(message: String, term: Term, frames: Iterable[Frame] = Iterable.empty[Frame]) extends RethinkError(message)
