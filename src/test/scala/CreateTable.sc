/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 3/23/13
 * Time: 4:16 PM 
 */

import scala.concurrent._

import com.rethinkdb.Ast._
import com.rethinkdb.Connection
import ql2.{Ql2 => p}

val db = DB("foo")
val connection = new Connection("172.16.2.45")
/*
val builder= p.Term.newBuilder()
create.compile(builder)
builder.build()  */
val future = db.table_create("bar") ! connection

blocking(future)


