package com.rethinkdb

import com.rethinkdb.Ast.{Term, DB}
import netty.Socket
import ql2.{Ql2=>p}

import concurrent.Future
import java.util.concurrent.atomic.AtomicLong
import org.jboss.netty.channel.socket.nio.NioClientSocketChannelFactory
import java.util.concurrent.Executors
import org.jboss.netty.bootstrap.ClientBootstrap
import java.net.InetSocketAddress


/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 3/23/13
 * Time: 12:25 PM 
 */

case class Connection(host:String="localhost",port:Int=28015,db:Either[String,DB]=Left("test")) {

  import com.rethinkdb.Conversions._
  private var _db:DB = db match{
    case Left(name:String)=>DB(name)
    case Right(b:DB)=>b
  }
  private val token:AtomicLong = new AtomicLong()

  lazy val socket = Socket(host,port)
  def _query(term:Term)={

    val query = p.Query.newBuilder()
    query.setType(p.Query.QueryType.START).setToken(token.incrementAndGet())

    // TODO : Support global args but for now just set the `db`

    query.addGlobalOptargs(_db)


    term.compile(query.getQueryBuilder)


    socket.write(query.build())
    //_db.compile(pair.setKey("db").getValBuilder)

  }
}
