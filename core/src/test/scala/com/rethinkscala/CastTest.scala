package com.rethinkscala

import org.scalatest.FunSuite
import Blocking.functional._

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 3/25/2015
 * Time: 3:19 PM 
 */
case class CastBean(name:String,values:List[Int])
class CastTest extends FunSuite with WithBase{

  test("sequence casting"){
    val cast = table.to[CastBean]

    cast.insert(CastBean("foo",List(1,2,3,4))).run

    val values = cast(0) \ "values"

    val results = values.toSeq[Int].run




  }
}
