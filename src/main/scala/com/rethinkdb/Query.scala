package com.rethinkdb


import com.rethinkdb.utils.Helpers._


/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 5/27/13
 * Time: 9:52 AM
 * To change this template use File | Settings | File Templates.
 */
case class Query(term:Term,connection:Connection) {


   lazy val ast:ql2.Term = term.ast

   def execute()={
     val s = connection.socket
     val token = connection.token.getAndIncrement
     s.write(toQuery(term,token),term)

   }
}
