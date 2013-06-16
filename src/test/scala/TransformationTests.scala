import org.scalatest.FunSuite

import com.rethinkscala._
import ast._
import com.rethinkscala.Implicits._

class TransformationTests extends FunSuite{

  test("test map"){
    val term=r.table("marvel").map((hero:Var)=> hero \ "combatPower" + hero \ "combatPower" * 2)

    val ast = term.ast

    println(ast)

  }
}
