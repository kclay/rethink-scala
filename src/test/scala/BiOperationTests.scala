import org.scalatest.FunSuite

import com.rethinkdb._
import ast._

class BiOperationTests extends FunSuite{

  test("TermNode.add"){

    val addNum = Expr(1)+2

    var term = addNum.toInternalTerm


    assert(addNum.isInstanceOf[Add])
    assert(term.`args`.size == 2)
    assert(term.`args`(0).`datum`.get.`rNum`== Some(1.0))
    assert(term.`args`(1).`datum`.get.`rNum`== Some(2.0))


     val addStr = Expr("hello")+="world"

    term = addStr.toInternalTerm

    assert(term.`args`.size ==2)
    assert(term.`args`(0).`datum`.get.`rStr`== Some("hello"))
    assert(term.`args`(1).`datum`.get.`rStr`==Some("world"))

  }
}
