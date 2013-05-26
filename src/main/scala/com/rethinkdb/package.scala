package com

import com.rethinkdb.ast._
import com.rethinkdb.ast.StringDatum
import com.rethinkdb.ast.NumberDatum
import scala.Some
import com.rethinkdb.ast.BooleanDatum


/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 3/19/13
 * Time: 7:32 PM 
 */
package object rethinkdb {



  implicit def boolToDataNum(b: Boolean): Term= BooleanDatum(b)

  implicit def intToDatNum(i: Int): Term = NumberDatum(i)

  implicit def longToDatNum(l: Long): Term = NumberDatum(l)

  implicit def floatToDatNum(f: Float): Term= NumberDatum(f)

  implicit def string2DatNum(s: String): StringDatum = StringDatum(s)

  implicit def bool2Option(value: Boolean): Option[Boolean] = Some(value)

  implicit def string2Option(value: String): Option[String] = Some(value)
  implicit def double2Option(value: Double): Option[Double] = Some(value)
  implicit def int2Option(value: Int): Option[Int] = Some(value)

  implicit def string2DB(name: String): DB = DB(name)


}
