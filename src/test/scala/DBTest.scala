import org.scalatest.FunSuite

import com.rethinkdb._
import com.rethinkdb.ast._
import ql2.{Ql2=>p}
import p.Term.TermType
class DBTest extends FunSuite{


  test("create db instance"){
   val db = DB("foo")

    val term = db.toTerm

    assert(term.getType == TermType.DB)
    assert(term.getArgsCount == 1)
    assert(term.getArgs(0).getType == TermType.DATUM)
    assert(term.getArgs(0).getDatum.getRStr=="foo")

  }
  test("db with table"){


    val db = DB("foo")
    val term = db.table("bar",true).toTerm
     System.out.println(term)
    assert(term.getType ==TermType.TABLE)
    assert(term.getArgsCount == 2)
    assert(term.getArgs(0).getType == TermType.DB)
    assert(term.getArgs(0).getArgs(0).getDatum.getRStr=="foo")

    assert(term.getArgs(1).getType == TermType.DATUM)
    assert(term.getArgs(1).getDatum.getRStr=="bar")

    assert(term.getOptargsCount==1)
    assert(term.getOptargs(0).getKey=="use_outdated")
    assert(term.getOptargs(0).getVal.getDatum.getRBool == true)
   // asser(term.getArgsCount)
  }
}
