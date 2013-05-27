import org.scalatest.FunSuite

import com.rethinkdb._
import ast._

class ExprTests extends FunSuite {

  test("auto casting") {

    assert(Expr(1).isInstanceOf[NumberDatum])
    assert(Expr(1L).isInstanceOf[NumberDatum])
    assert(Expr("hello").isInstanceOf[StringDatum])
    assert(Expr(Seq(1, 2, 3)).isInstanceOf[MakeArray])
    assert(Expr(Map.empty[String, Boolean]).isInstanceOf[MakeObj])




  }

  test("map conversion"){
    val map = Seq(("foo", "bar"), ("bar", 1), ("you", Seq(1, 2, 3))).toMap

    val objTerm = Expr(map).toInternalTerm

    val optargs = objTerm.`optargs`

    assert(optargs.size == 3)
    assert(optargs(0).key.get == "foo")
    assert(optargs(0).str.get == "bar")

    assert(optargs(1).key.get == "bar")
    assert(optargs(1).num.get == 1)
    assert(optargs(2).key.get == "you")

    val array = optargs(2).array

    array.left.get.zipWithIndex foreach {e=> assert(e._1.num.get == e._2+1)}
  }
}
