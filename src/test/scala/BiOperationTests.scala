import org.scalatest.FunSuite

import com.rethinkdb._
import ast._

class BiOperationTests extends FunSuite{

  test("TermNode.add"){

    val addNum = Expr(1)+2

    var term = addNum.toTerm

    assert(addNum.isInstanceOf[Add])
    assert(term.getArgsCount ==2)
    assert(term.getArgs(0).getDatum.getRNum == 1)
    assert(term.getArgs(1).getDatum.getRNum==2)



    val addStr = Expr("hello") +" world"

    term = addStr.toTerm

    assert(term.getArgsCount == 2)
    assert(term.getArgs(0).getDatum.getRStr=="hello")
    assert(term.getArgs(1).getDatum.getRStr==" world")

  }
}
