package com.rethinkdb.ast

import com.rethinkdb.Term

trait CanProduce[T <: CanProduce[T]] {
  self: T =>
  type ResultType >: self.type <: T

}

trait Produce extends Term {


  import scala.reflect.runtime.universe._

  def withResult[A: TypeTag](result: A): Result =
    if (withMapProduce) extract[A, Map[String, Any]](result, defaultValue)(fromMap _)
    else result.asInstanceOf[Result]

  val withMapProduce = false

  def wrap(value: Any): Result = AnyResult(value)


  def defaultValue: Result = AnyResult(None)

  protected def fromMap(m: Map[String, Any]): Any = defaultValue

  protected def extract[A: TypeTag, T: TypeTag](result: A, defaultValue: Result)(f: T => Any): Result = {
    typeOf[A] match {
      case t if t =:= typeOf[T] => wrap(f(result.asInstanceOf[T]))
      case _ => defaultValue
    }
  }


}

sealed trait Feature


trait Addable extends Feature

trait Comparable extends Feature

trait Sequence extends Feature

trait Document extends Feature

trait Binary extends Feature

trait Numeric extends Feature with Addable

trait Math extends Numeric with Addable

trait RAnyRef extends Numeric with Binary with Document with Sequence with Comparable with Addable


trait Result {

  def unwrap
}


case class BooleanResult(value: Boolean) extends Result {

  def unwrap = value
}

case class NumericResult(value: Double) extends Result {
  def unwrap = value
}

case class StringResult(value: String) extends Result {
  def unwrap = value
}

case class IterableResult[R](value: Iterable[R]) extends Result {
  def unwrap = value
}

case class DocumentResult(doc: Document) extends Result {
  def unwrap = doc
}

case class AnyResult(value: Any) extends Result {
  def unwrap = value
}

trait ProduceSequence extends Produce with Sequence {


  override def defaultValue: Result = IterableResult[Any](Iterable.empty[Any])
}

trait ProduceComparable

trait ProduceBinary extends Produce with Binary {


  override def wrap(value: Any): Result = BooleanResult(value.asInstanceOf[Boolean])

  override def defaultValue: Result = BooleanResult(false)
}

trait ProduceLiteral extends ProduceComparable with Addable with Comparable

trait ProduceDocument extends Produce {


  override def wrap(value: Any): Result = DocumentResult(value.asInstanceOf[Document])

  override def defaultValue: Result = DocumentResult(new Document {})

}

trait ProduceNumeric extends Produce with ProduceLiteral with Numeric {

  type ResultType = NumericResult

  override def wrap(value: Any): Result = {

    val d = value match {
      case d: Double => d
      case i: Int => i.toDouble
      case _ => 0
    }
    NumericResult(d)
  }

  override def defaultValue: Result = NumericResult(0)

}

trait ProductMath extends Product  with ProduceNumeric with WithAddition with WithNumeric

trait ProduceString extends Produce with ProduceLiteral {


  override def wrap(value: Any): Result = StringResult(value.toString)

  override def defaultValue: Result = StringResult("")
}


trait ProduceAny extends Sequence with ProduceBinary with ProduceString with ProduceNumeric with ProduceDocument {


  override def wrap(value: Any): Result = AnyResult(value)

  override def defaultValue: Result = AnyResult(None)
}


sealed trait LogicSignature


trait WithTable extends LogicSignature {

  self: Table =>


  def <<(other: Table, func: Predicate2) = innerJoin(other, func)

  def innerJoin(other: Table, func: Predicate2) = InnerJoin(this, other, func)

  def outerJoin(other: Table, func: Predicate2) = OuterJoin(this, other, func)

  def >>(other: Table, func: Predicate2) = outerJoin(other, func)

  def >>=(attr: String, other: Table, index: Option[String] = None) = eqJoin(attr, other, index)

  def eqJoin(attr: String, other: Table, index: Option[String] = None) = EqJoin(this, other, attr, index)


}

trait WithSequence extends LogicSignature {
  self: Sequence =>
  def pluck(attrs: String*) = Pluck(this, attrs)

  def without(attrs: String*) = Without(this, attrs)

  def map(func: Predicate1) = RMap(this, func)

  def concatMap(func: Predicate1) = ConcatMap(this, func)

  def order(keys: Ordering*) = OrderBy(this, keys)

  def skip(amount: Int) = Skip(this, amount)

  def slice(start: Int = 0, end: Int = -1) = Slice(this, start, end)

  def apply(prange: SliceRange) = Slice(this, prange.start, prange.end)

  def union(sequences: Sequence) = Union(this, sequences)


}

trait WithDocument extends LogicSignature {
  self: ProduceDocument =>

  def attr(name: String) = GetAttr(this, name)

  def \(name: String) = attr(name)

  def append(value: Any) = Append(this, value)

  def :+(value: Any) = append(value)

  def merge(other: Document) = Merge(this, other)

  def +(other: Document) = merge(other)

  def contains(attr: String) = Contains(this, attr)


}


trait WithComparable extends LogicSignature {
  self: Term with Comparable =>


  def ==(other: Comparable) = eq(other)

  def eq(other: Comparable) = Eq(this, other)

  def !=(other: Comparable) = ne(other)

  def ne(other: Comparable) = Ne(this, other)

  def <(other: Comparable) = lt(other)

  def lt(other: Comparable) = Lt(this, other)

  def <=(other: Comparable) = le(other)

  def le(other: Comparable) = Le(this, other)

  def >(other: Comparable) = gt(other)

  def gt(other: Comparable) = Gt(this, other)

  def >=(other: Comparable) = ge(other)

  def ge(other: Comparable) = Ge(this, other)


  //def contains(attribute: String*) = Contains(this, attribute)

}

trait WithUnary extends LogicSignature {
  self: Term =>

  def ~(other: Term) = Not(this)

}


trait WithAddition extends LogicSignature {
  self: ProduceLiteral =>

  def +(other: Addable) = add(other)

  def add(other: Addable) = Add(this, other)

  def +=(other: Addable) = Add(this, other)

  //def >+(other: AcceptType) = Add(other, this)

}


trait WithNumeric extends LogicSignature {
  self: ProduceNumeric =>


  def -(other: Numeric) = Sub(this, other)

  //def >-(other: Numeric) = Sub(other, this)

  def *(other: Numeric) = Mul(this, other)

  //def >*(other: Numeric) = Mul(other, this)

  def /(other: Numeric) = Div(this, other)

  // def >/(other: Numeric) = Div(other, this)

  def %(other: Numeric) = Mod(this, other)

  //def >%(other: AcceptType) = Mod(other, this)


}

trait WithBinary extends LogicSignature {
  self: ProduceBinary with Binary =>


  def &(other: Binary) = All(this, other)

  def &&(other: Binary) = All(other, this)

  def &>(other: Binary) = this && other

  // or
  def |(other: Binary) = RAny(this, other)

  // right or
  def >|(other: Binary) = RAny(other, this)


  // def row(name: String) = Core.row \ name

}

trait WithAny extends Term with WithBinary with WithNumeric with WithAddition with WithUnary with WithComparable
                      with WithDocument with RAnyRef with ProduceAny {
  self: ProduceAny =>
}