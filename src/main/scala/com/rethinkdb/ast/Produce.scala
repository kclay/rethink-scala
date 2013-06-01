package com.rethinkdb.ast

import com.rethinkdb.Term

sealed trait Produce extends Term

sealed trait Feature

trait Addable extends Feature


trait Comparable extends Feature


trait ProduceSequence extends Produce with WithTransformation

trait ProduceComparable extends Produce

trait ProduceBinary extends Produce

trait ProduceLiteral extends ProduceComparable with Addable with Comparable

trait ProduceDocument extends ProduceSequence

trait ProduceNumeric extends Produce with ProduceLiteral

trait ProduceString extends Produce with ProduceLiteral


trait ProduceAny extends ProduceSequence with ProduceBinary with ProduceString with ProduceNumeric with ProduceDocument


sealed trait LogicSignature {

  import scala.reflect.runtime.universe._

  type AcceptType = Term

  lazy val acceptTypeOf = typeOf[AcceptType]

  def accepts[T: TypeTag](value: T) = typeOf[T] match {
    case t if t =:= acceptTypeOf => true
    case _ => false
  }


}

trait TermSignature[T] extends LogicSignature {
  override type AcceptType = Term with T
}


trait WithTable extends TermSignature[Table]{
  self:Table=>

  def <<(other: Table, func: Predicate2) = innerJoin(other, func)

  def innerJoin(other: Table, func: Predicate2) = InnerJoin(this, other, func)

  def outerJoin(other: Table, func: Predicate2) = OuterJoin(this, other, func)

  def >>(other: Table, func: Predicate2) = outerJoin(other, func)

  def >>=(attr: String, other: Table, index: Option[String] = None) = eqJoin(attr, other, index)

  def eqJoin(attr: String, other: Table, index: Option[String] = None) = EqJoin(this, other, attr, index)


}
trait WithSequence extends LogicSignature{
  self:ProduceSequence =>
  def pluck(attrs:String*) = Pluck(this,attrs)
  def without(attrs:String*)=Without(this,attrs)

  def map(func: Predicate1) = RMap(this, func)

  def concatMap(func: Predicate1) = ConcatMap(this, func)

  def order(keys: Ordering*) = OrderBy(this, keys)

  def skip(amount: Int) = Skip(this, amount)

  def slice(start: Int = 0, end: Int = -1) = Slice(this, start, end)

  def apply(prange: SliceRange) = Slice(this, prange.start, prange.end)

  def union(sequences: ProduceSequence*) = Union(this, sequences))


}
trait WithDocument extends TermSignature[ProduceDocument] {
  self: ProduceDocument =>

  def attr(name: String) = GetAttr(this, name)

  def \(name: String) = attr(name)

  def append(value: Any) = Append(this, value)

  def :+(value: Any) = append(value)
  def merge(other:AcceptType) = Merge(this,other)
  def +(other:AcceptType) = merge(other)
  def contains(attr:String) = Contains(this,attr)



}



trait WithComparable extends TermSignature[Comparable] {
  self: Term with Comparable =>

  def ==(other: AcceptType) = eq(other)

  def eq(other: AcceptType) = Eq(this, other)

  def !=(other: AcceptType) = ne(other)

  def ne(other: AcceptType) = Ne(this, other)

  def <(other: AcceptType) = lt(other)

  def lt(other: AcceptType) = Lt(this, other)

  def <=(other: AcceptType) = le(other)

  def le(other: AcceptType) = Le(this, other)

  def >(other: AcceptType) = gt(other)

  def gt(other: AcceptType) = Gt(this, other)

  def >=(other: AcceptType) = ge(other)

  def ge(other: AcceptType) = Ge(this, other)


  //def contains(attribute: String*) = Contains(this, attribute)

}

trait WithUnary extends LogicSignature {
  self: Term =>
  def ~(other: AcceptType) = Not(this)

}


trait WithAddition extends TermSignature[Addable] {
  self: ProduceLiteral =>

  def +(other: AcceptType) = add(other)

  def add(other: AcceptType) = Add(this, other)

  def +=(other: AcceptType) = Add(this, other)

  def >+(other: AcceptType) = Add(other, this)

}



trait WithNumeric extends TermSignature[ProduceNumeric] {
  self: ProduceNumeric =>


  def -(other: AcceptType) = Sub(this, other)

  def >-(other: AcceptType) = Sub(other, this)

  def *(other: AcceptType) = Mul(this, other)

  def >*(other: AcceptType) = Mul(other, this)

  def /(other: AcceptType) = Div(this, other)

  def >/(other: AcceptType) = Div(other, this)

  def %(other: AcceptType) = Mod(this, other)

  def >%(other: AcceptType) = Mod(other, this)
}

trait WithBinary extends TermSignature[ProduceBinary] {
  self: ProduceBinary =>

  def &(other: AcceptType) = All(this, other)

  def &&(other: AcceptType) = All(other, this)

  def &>(other: AcceptType) = this && other

  // or
  def |(other: AcceptType) = RAny(this, other)

  // right or
  def >|(other: AcceptType) = RAny(other, this)


  // def row(name: String) = Core.row \ name

}

trait WithAny extends WithBinary with WithNumeric with WithAddition with WithUnary with WithComparable with WithDocument {
  self: ProduceAny =>
}