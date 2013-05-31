package com.rethinkdb.ast

import com.rethinkdb.Term
import ql2.Term.TermType.EnumVal
import ql2.Term.TermType

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 5/28/13                                                               v
 * Time: 5:36 PM
 * To change this template use File | Settings | File Templates.
 */


case class PRange(start: Int = 0, end: Int = -1)

trait WithTransformations extends ProducesSequence{
  self: Term =>




  def map(func: Predicate1) = RMap(this, func)

  def concatMap(func: Predicate1) = ConcatMap(this, func)

  def order(keys: Ordering*) = OrderBy(this, keys)

  def skip(amount: Int) = Skip(this, amount)

  def slice(start:Int=0,end:Int= -1)=Slice(this,start,end)

  def apply(prange: PRange) = Slice(this, prange.start, prange.end)

  def union(sequences:ProducesSequence*)=Union(this,sequences)



}


abstract class Transformation extends Term  with WithTransformations{
  val target: Term
  val func: Predicate

  override lazy val args = buildArgs(target, func.apply())


}


sealed trait TransformSequence extends WithTransformations with Term{

}


case class RMap(target: ProducesSequence, func: Predicate1) extends TransformSequence {

  def termType: EnumVal = TermType.MAP

  def toConcat = ConcatMap(target, func)

}

case class ConcatMap(target: ProducesSequence, func: Predicate1) extends TransformSequence {

  def termType: EnumVal = TermType.CONCATMAP

  def toMap = Map(target, func)
}

abstract class Ordering extends Term {
  val attr: String
  override lazy val args: Seq[Term] = buildArgs(attr)
}

case class Asc(attr: String) extends Ordering {
  def termType: EnumVal = TermType.ASC
}

case class Desc(attr: String) extends Ordering {
  def termType: EnumVal = TermType.DESC
}

case class OrderBy(target: ProducesSequence, keys: Seq[Ordering]) extends TransformSequence{
  def termType: EnumVal = TermType.ORDERBY
}


case class Skip(target: ProducesSequence, index: Int) extends Term {
  def termType: EnumVal = TermType.SKIP
}
case class Union(target:ProducesSequence,others:Seq[ProducesSequence]) extends  TransformSequence{

  override lazy val args: Seq[Term] = buildArgs((others.+:(target)):_*)

  def termType: EnumVal = TermType.UNION
}
