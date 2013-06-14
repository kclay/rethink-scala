package com.rethinkdb


import com.rethinkdb.ast.Produce
import scala.concurrent.Future


case class Query(term:Produce,connection:Connection) {

   type ResultType =term.ResultType

   lazy val ast:ql2.Term = term.ast

   def execute:Future[ResultType]={

     val token = connection.token.getAndIncrement
     connection.write[ResultType](term)

   }
}
