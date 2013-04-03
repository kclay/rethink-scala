package com

import rethinkdb.ast._
import rethinkdb.ast.BooleanDatum
import rethinkdb.ast.DB
import rethinkdb.ast.NumberDatum
import scala.Some

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 3/19/13
 * Time: 7:32 PM 
 */
package object rethinkdb {



  implicit def boolToDataNum(b: Boolean):Datum = BooleanDatum(b)

  implicit def intToDatNum(i: Int):Datum = NumberDatum(i)

  implicit def longToDatNum(l: Long):Datum = NumberDatum(l)

  implicit def floatToDatNum(f: Float):Datum = NumberDatum(f)
  implicit  def string2DatNum(s:String):Datum = StringDatum(s)

  implicit def bool2Option(value: Boolean):Option[Boolean] = Some(value)

  implicit def string2Option(value: String):Option[String] = Some(value)

  implicit def int2Option(value: Int):Option[Int] = Some(value)

  implicit def string2DB(name:String):DB = DB(name)







  //implicit def stringToDatNum(s: String) = Datum(s)

}
