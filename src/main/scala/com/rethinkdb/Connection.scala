package com.rethinkdb

import ast.{WithDB, DB}


import ql2.{Query}

import java.util.concurrent.atomic.AtomicLong
import com.rethinkdb.netty.AsyncSocket
import com.rethinkdb.ConvertTo._


/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 3/23/13
 * Time: 12:25 PM 
 */

object Connection {

  lazy val defaultConnection = Connection


}

class Connection(host: String = "localhost", port: Int = 28015, maxConnections: Int = 5) {


  private var db: DB = DB("test")
  /*
   private var _db:DB = db match{
     case Left(name:String)=>DB(name)
     case Right(b:DB)=>b
   }
   */
  private val token: AtomicLong = new AtomicLong()


  lazy val socket = AsyncSocket(host, port, maxConnections)


  def ?(term: Term) = {


    val query = Some(
      Query().setType(Query.QueryType.START)
        .setQuery(term.toInternalTerm).setToken(token.incrementAndGet())

    ).map(q => {
      term match {
       // case d: WithDB => d.scopedDB.map(q.addAllGlobalOptargs(Query.AssocPair(Some("db"),Some(_.))))
        case _ => q
      }

    }).get


    socket.write(query, term)
    //_db.compile(pair.setKey("db").getValBuilder)

  }
}
