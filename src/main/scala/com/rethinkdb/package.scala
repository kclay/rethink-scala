package com

import rethinkdb.ast.{BooleanDatum, NumberDatum}

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 3/19/13
 * Time: 7:32 PM 
 */
package object rethinkdb {


  implicit def boolToDataNum(b: Boolean) = BooleanDatum(b)

  implicit def intToDatNum(i: Int) = NumberDatum(i)

  implicit def longToDatNum(l: Long) = NumberDatum(l)

  implicit def floatToDatNum(f: Float) = NumberDatum(f)

  implicit def bool2Option(value: Boolean) = Some(value)

  implicit def string2Option(value: String) = Some(value)

  implicit def int2Option(value: Int) = Some(value)

  //implicit def stringToDatNum(s: String) = Datum(s)

}
