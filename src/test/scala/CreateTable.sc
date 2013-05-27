/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 3/23/13
 * Time: 4:16 PM 
 */

import com.rethinkdb.ast.DB
























































import com.rethinkdb.utils.Helpers.toQuery
import scala.concurrent._
import com.rethinkdb.Connection
val db = DB("test")
val connection = new Connection("172.16.2.45")
val query = toQuery(db.newTable("bar") ,1)


//println(query)
val future = db.newTable("bar") ! connection
blocking(future)

