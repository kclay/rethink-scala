import org.scalatest.FunSuite

import com.rethinkdb._
import ast._
import com.rethinkdb.ast.Functional._

class TransformationTests extends FunSuite{

  test("test map"){
    val term=r.table("marvel").map((hero:Var)=> hero \ "combatPower" + hero \ "combatPower" * 2)

    val ast = term.ast

    println(ast)

  }
}
