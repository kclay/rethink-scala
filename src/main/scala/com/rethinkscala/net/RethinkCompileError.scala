package com.rethinkscala.net

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 7/3/13
 * Time: 5:21 PM
 *
 */
case class RethinkCompileError(message: String, term: Term, frames: Iterable[Frame]) extends RethinkError(message)
