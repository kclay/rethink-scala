package com.rethinkdb.ast

import com.rethinkdb.Term

sealed trait Produce{

}
trait ProduceSequence extends Produce{
  self:Term=>
}