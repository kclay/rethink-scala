package com.rethinkdb

import ql2.Ql2._
import scala.concurrent.ExecutionContext.Implicits.global
import ql2.Ql2.Term;
import ql2.Ql2.Query.AssocPair

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 3/19/13
 * Time: 12:06 PM 
 */



trait TermBase{
  private var args:Seq[TermBase] = Seq.empty[TermBase]
  private var optargs:Seq[(String,DataNum)]=Seq.empty[(String,DataNum)]
  val termType:Term.TermType
  def run()={
    None
  }

  implicit def termBaseToTerm(base:TermBase):Term = {

    base.compile(Term.newBuilder().build())


  }
  implicit def optargToAssocPair(i:(String,DataNum)):AssocPair={

    AssocPair.newBuilder().setKey(i._1).setVal(i._2).build()
  }
  def compile(term:Term):Term={
    val builder = term.toBuilder.setType(termType)
    for (a <- args)builder.addArgs(a)
    //for (o <- optargs) builder.addOptargs(o)
    builder.build()


  }
}
trait BaseQuery {


  val term: Term.TermType


}

class T(args: Seq[AnyRef], inspect: String = "") extends Iterator[AnyRef] {
  var last = "0"

  // mutable state
  def hasNext = true // the twitter stream has no end

  def next() = {
    for (a <- args) yield a
  }
}


sealed trait ExprWrap;


abstract class DataNum(data:Any) extends TermBase{

  implicit def boolToDatNum
  override def compile(term:Term):Term={
    val builder = term.toBuilder
    val a:Boolean =true
    val datumBuilder = builder.getDatumBuilder
    data match{
      case None=>
      case b:Boolean=>datumBuilder.setType(Datum.DatumType.R_BOOL).setRBool(b)
      case i @(Int | Float | Long) =>datumBuilder.setType(Datum.DatumType.R_NUM).setRNum(i match{
        case a:Int =>a.toDouble
        case b:Float => b.toDouble

        case c:Long => c.toDouble
      })
      case s:String => datumBuilder.setType(Datum.DatumType.R_STR).setRStr(s)
      case _=> throw new RuntimeException("Cannot build a query")

    }
    builder.build()
  }
}

trait DatumApplier{

  def apply(d:Datum)
}


object DataNum{


}

case class Var(name: String) {
  val term = Term.TermType.VAR
}

class JavaScript extends BaseQuery {
  val term = Term.TermType.JAVASCRIPT
}



case class Eq extends BaseQuery {
  val term = Term.TermType.EQ
}

class Ne extends BaseQuery {
  val term = Term.TermType.NE
}

class Lt extends BaseQuery {
  val term = Term.TermType.LT
}

class Le extends BaseQuery {
  val term = Term.TermType.LE
}

