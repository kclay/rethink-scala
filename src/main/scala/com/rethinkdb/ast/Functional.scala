package com.rethinkdb.ast

import com.rethinkdb.{Term, TermMessage}

import ql2.Term.TermType
import java.util.concurrent.atomic.AtomicInteger
import ql2.Term.TermType.EnumVal

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 4/2/13
 * Time: 7:41 PM
 * To change this template use File | Settings | File Templates.
 */

case class Get(from: Term, attribute: String) extends TermMessage {
  override lazy val args = buildArgs(from, attribute)

  def termType = TermType.GET
}


object Predicate {
  private val _nextVarId = new AtomicInteger()

  def nextVarId = _nextVarId.incrementAndGet()
}


trait WithFilter {

  def filter(fields: Map[String, Any])

  def filter(term: Term)

  def filter(f: Predicate1)
}

object Functional {
  implicit def toPredicate1(f: (Var) => Term) = new Predicate1(f)

}

trait Functional {

  def lambda(f: (Var) => Term) = new Predicate1(f)

  def filter(attrs: Map[String, Any])


}

abstract class Predicate extends {

  val amount: Int


  protected def _invoke(v: Var*): Term

  private[rethinkdb] def invoke: Seq[Term] = {
    val (ids, vars) = take(amount)
    val product = _invoke(vars: _*)
    Seq(MakeArray(ids), product)
  }

  protected def take(amount: Int): (Seq[Int], Seq[Var]) = {
    val ids = for (i <- 1 to amount) yield Predicate.nextVarId
    val vars = ids.map(Var(_))
    (ids, vars)
  }

  def apply(): Term = Func(this)


}

case class Func(f: Predicate) extends Term {

  override lazy val args = buildArgs(f.invoke: _*)

  def termType: EnumVal = TermType.FUNC
}

case class FuncCall(f: Predicate, target: Term) extends Term {

  def termType: EnumVal = TermType.FUNCALL
}


case class Predicate1(f: (Var) => Term) extends Predicate {


  protected def _invoke(vars: Var*) = f(vars(0))


  val amount: Int = 1
}

case class Predicate2(f: (Var, Var) => Term) extends Predicate {

  protected def _invoke(v: Var*) = f(v(0), v(1))


  val amount: Int = 2
}

case class Predicate3(f: (Var, Var, Var) => Term) extends Predicate {

  protected def _invoke(v: Var*) = f(v(0), v(1), v(2))


  val amount: Int = 2
}

//class Functional {


// TODO : Reduce

// TODO : Map
// TODO : Filter
// TODO : ConcatMap
// TODO : OrderBy
// TODO : Distinct
// TODO : Count
// TODO : Union
// TODO : Nth
// TODO : GroupedMapReduce
// TODO : GroupBy
// TODO : InnerJoin
// TODO : OuterJoin
// TODO : EqJoin
// TODO : Zip
// TODO : CoerceTo
// TODO : TypeOf
// TODO : Update  ^
// TODO : Delete
// TODO : Replace
// TODO : Insert


//}
