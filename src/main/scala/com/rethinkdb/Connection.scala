package com.rethinkdb

import ast.{WithDB, DB}


import ql2.{Ql2 => p}

import java.util.concurrent.atomic.AtomicLong
import com.rethinkdb.conversions.Tokens._
import com.rethinkdb.netty.AsyncSocket


/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 3/23/13
 * Time: 12:25 PM 
 */

object Connection {

  lazy val defaultConnection = Connection


}

class Connection(host: String = "localhost", port: Int = 28015,maxConnections:Int=5) {



  private var db: DB = DB("test")
  /*
   private var _db:DB = db match{
     case Left(name:String)=>DB(name)
     case Right(b:DB)=>b
   }
   */
  private val token: AtomicLong = new AtomicLong()


  lazy val  socket = AsyncSocket(host, port,maxConnections)


  def ?(term: Term) = {

    val query = p.Query.newBuilder()
    query.setType(p.Query.QueryType.START).setToken(token.incrementAndGet())

    // TODO : Support global args but for now just set the `db`
    if (term.isInstanceOf[WithDB]) {
      term.asInstanceOf[WithDB].scopedDB.map(query.addGlobalOptargs(_))
    }



    term.compile(query.getQueryBuilder)


    socket.write(query.build(),term)
    //_db.compile(pair.setKey("db").getValBuilder)

  }
}
