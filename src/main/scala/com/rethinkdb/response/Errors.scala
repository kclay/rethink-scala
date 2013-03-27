package com.rethinkdb.response

import com.rethinkdb.Ast.Term

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 3/26/13                                                             m
 * Time: 9:36 PM
 * To change this template use File | Settings | File Templates.
 *
 */
abstract class RethinkError(message:String) extends Exception(message){

  val term:Term
  val frames:Iterable[Frame]
}
//abstract class RethinkError(message:String,term:Term,frames:Iterable[Frame]) extends Exception(message)
case class RethinkRuntimeError(message:String,term:Term,frames:Iterable[Frame]) extends RethinkError(message)
case class RethinkCompileError(message:String,term:Term,frames:Iterable[Frame]) extends RethinkError(message)


case class RethinkClientError(message:String,term:Term,frames:Iterable[Frame]) extends RethinkError(message)




