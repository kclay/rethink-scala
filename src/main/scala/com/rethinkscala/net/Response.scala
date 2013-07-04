package com.rethinkscala.net

import com.rethinkscala.Term

case class RethinkClientError(message: String, term: Term, frames: Iterable[Frame]) extends RethinkError(message)

