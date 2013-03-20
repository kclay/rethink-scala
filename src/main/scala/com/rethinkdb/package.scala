package com

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 3/19/13
 * Time: 7:32 PM 
 */
package object rethinkdb {

  import com.rethinkdb.Ast.DataNum

  implicit def boolToDataNum(b: Boolean): DataNum = DataNum(b)

  implicit def intToDataNum(i: Int) = DataNum(i)

  implicit def longToDataNum(l: Long) = DataNum(l)

  implicit def floatToDataNum(f: Float) = DataNum(f)

  implicit def stringToDataNum(s: String) = DataNum(s)

}
