package com.rethinkdb

import com.rethinkdb.{DataNum,DB}

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 3/19/13
 * Time: 12:02 PM 
 */
case class Connection(host: String, port: Int, db: String = "test")
  private val _db =DB(db)

object Connection {


}

