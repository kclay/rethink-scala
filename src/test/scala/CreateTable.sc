/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 3/23/13
 * Time: 4:16 PM 
 */

import scala.concurrent._

import com.rethinkdb.Ast._
import com.rethinkdb.Connection
val db=DB("foo")
val connection=new Connection()
val future = TableCreate("bar") ! connection


val response = blocking(future)
response

