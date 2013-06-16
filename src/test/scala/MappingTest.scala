import org.scalatest.FunSuite

import com.rethinkscala._
import com.rethinkscala.ast._
import ql2.{Ql2=>p}

class MappingTest extends FunSuite{


  test("should return InfoResult"){

    val db = DB("test")

    val version =new Version1("172.16.2.45")
     implicit val connection = new Connection(version)


    val info =db.table("bar").info

    val result = info.as[InfoResult]


    //val r = info.run





    println(result)
  }

  test("should return TableResult"){

    val db = DB("test")

    val version =new Version1("172.16.2.45")
    implicit val connection = new Connection(version)


    val info =db.table("bar").info

    val result = info.as[TableInfoResult]

    println(result)
  }

}
