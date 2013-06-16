package com.rethinkscala.ast

import com.rethinkscala.{ Term, TermMessage }

import ql2.Term.TermType
import java.util.concurrent.atomic.AtomicInteger
import ql2.Term.TermType.EnumVal

case class Get(from: Term, attribute: String) extends TermMessage with ProduceDocument {
  override lazy val args = buildArgs(from, attribute)

  def termType = TermType.GET
}

object Predicate {
  private val _nextVarId = new AtomicInteger()

  def nextVarId = _nextVarId.incrementAndGet()
}

abstract class Predicate extends {

  val amount: Int

  protected def _invoke(v: Seq[Var]): Typed

  private[rethinkscala] def invoke: Seq[Typed] = {
    val (ids, vars) = take(amount)
    val product = _invoke(vars)
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

case class Predicate1(f: (Var) => Typed) extends Predicate {

  protected def _invoke(vars: Seq[Var]) = f(vars(0))

  val amount: Int = 1
}

case class Predicate2(f: (Var, Var) => Typed) extends Predicate {

  protected def _invoke(v: Seq[Var]) = f(v(0), v(1))

  val amount: Int = 2
}

trait BooleanPredicate extends Predicate
case class BooleanPredicate1(f: (Var) => Binary) extends BooleanPredicate {

  protected def _invoke(vars: Seq[Var]) = f(vars(0))

  val amount: Int = 1
}

case class BooleanPredicate2(f: (Var, Var) => Binary) extends BooleanPredicate {
  protected def _invoke(v: Seq[Var]) = f(v(0), v(1))

  val amount: Int = 2
}

abstract class Transformation extends ProduceSequence {
  val target: Functional
  val func: Predicate

  override lazy val args = buildArgs(target, func())

}

/** Transform each element of the sequence by applying the given mapping function.
 *  @param target
 *  @param func
 */
case class RMap(target: Functional, func: Predicate1) extends Transformation {

  def termType: EnumVal = TermType.MAP

  def toConcat = ConcatMap(target, func)

}

/** Flattens a sequence of arrays returned by the mappingFunction into a single sequence.
 *  @param target
 *  @param func
 */
case class ConcatMap(target: Functional, func: Predicate1) extends Transformation {

  def termType: EnumVal = TermType.CONCATMAP

  def toMap = RMap(target, func)
}

case class Filter(target: Typed, func: Predicate) extends ProduceAny {
  def termType: EnumVal = TermType.FILTER
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
