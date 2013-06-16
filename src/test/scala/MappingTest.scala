import org.scalatest.FunSuite

import com.rethinkdb._
import com.rethinkdb.ast._
import ql2.{Ql2=>p}

class MappingTest extends FunSuite{


  test("should return InfoResult"){

    val db = DB("test")

    val version =new Version1("172.16.2.45")
     implicit val connection = new Connection(version)


    val info =db.table("bar").info

    val result = info.run[InfoResult]

   // info.run




    println(result)
  }

}
