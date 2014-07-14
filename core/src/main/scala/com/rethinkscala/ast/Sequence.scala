package com.rethinkscala.ast

import com.rethinkscala.{BoundOptions, _}
import com.rethinkscala.magnets.GroupFilterMagnet

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 2/24/14
 * Time: 8:15 AM 
 */


object Sequence {
  implicit def mapDocumentToSequence[T <: Document, ST, S[ST] <: Sequence[ST]] = CanMap[T, S[ST], ST]

  implicit class ScalaSequence[T](underlying: Sequence[T]) {

    def ++(sequence: Sequence[_]) = underlying.union(sequence)

    //def :::[B>:T](prefix:Sequence[B]) = prefix  merge underlying
  }

}

trait Aggregation[T] {


  val underlying = this

  def count() = Count(underlying)

  def count(value: String): Count = count(value: Datum)

  def count(value: Double): Count = count(value: Datum)

  def count(value: Boolean): Count = count(value: Datum)

  def count(value: Datum) = Count(underlying, value)

  def count(f: Var => Binary) = Count(underlying, f)

  def count(f: japi.BooleanPredicate) = Count(underlying, f)

  def sum() = Sum(underlying)

  def sum(value: String) = Sum(underlying, value)

  def sum(f: Var => Numeric) = Sum(underlying, f)

  def sum(f: japi.NumericPredicate) = Sum(underlying, f)


  def group[R](magnet: GroupFilterMagnet[R]): Group[R, T] = Group[R, T](underlying, magnet().map(FuncWrap(_)))

  def max() = Max(underlying)

  def max(field: String) = Max(underlying, field)

  def max(f: Var => Typed) = Max(underlying, f)

  def max(f: japi.Predicate) = Max(underlying, f)

  def min() = Max(underlying)

  def min(field: String) = Min(underlying, field)

  def min(f: japi.Predicate) = Min(underlying, f)

  def min(f: Var => Typed) = Min(underlying, f)

  def avg() = Avg(underlying)

  def avg(field: String) = Avg(underlying, field)

  def avg(f: japi.NumericPredicate) = Avg(underlying, f)

  def avg(f: Var => Numeric) = Avg(underlying, f)


  def distinct = Distinct(underlying)


  def contains(field: String, fields: String*) = Contains(underlying, fields.+:(field).map(FuncWrap(_)))

  def contains(field: Double, fields: Double*) = Contains(underlying, fields.+:(field).map(FuncWrap(_)))

  def contains(fields: Datum*) = Contains(underlying, fields.map(FuncWrap(_)))


  def contains(f: japi.BooleanPredicate) = Contains(underlying, Seq(f))


  def contains(f: Var => Binary) = Contains(underlying, Seq(f))

  def ?(attr: Datum) = contains(attr)

}

trait Sequence[T] extends ArrayTyped[T] with Multiply with Filterable[T] with Record with Aggregation[T] {


  override val underlying = this


  def indexesOf[R >: Datum](value: R) = IndexesOf(underlying, value)


  //def indexesOf(value: Binary): IndexesOf = indexesOf((x: Var) => value)

  def isEmpty = IsEmpty(underlying)

  def sample(amount: Int) = Sample(underlying, amount)

  def indexesOf(p: Var => Binary) = IndexesOf(underlying, p)

  def apply(index: Int) = Nth(underlying, index)

  def skip(amount: Int) = Skip(underlying, amount)

  def limit(amount: Int) = Limit(underlying, amount)

  def slice(start: Int = 0, end: Int = -1) = Slice(underlying, start, end, BoundOptions())

  def slice(start: Int, end: Int, bounds: BoundOptions) = if (end > 0) Slice(underlying, start, end, bounds)
  else
    Slice(underlying, List(start, end).max, -1, BoundOptions(rightBound = Some(Bound.Closed)))


  def apply(range: SliceRange) = slice(range.start, range.end)

  def union(sequence: Sequence[_]) = Union(underlying, sequence)


  def eqJoin[R](attr: String, other: Sequence[R]) = EqJoin(underlying, Left(attr), other)

  def eqJoin[R](attr: String, other: Sequence[R], index: String) = EqJoin(underlying, Left(attr), other, index)

  def eqJoin[R](func: Var => Typed, other: Sequence[R], index: String) = EqJoin(underlying, Right(func), other, index)

  def eqJoin[R](func: Var => Typed, other: Sequence[R]) = EqJoin(underlying, Right(func), other)

  def innerJoin[R](other: Sequence[R], func: (Var, Var) => Binary) = InnerJoin[T, R](underlying, other, func)

  def outerJoin[R](other: Sequence[R], func: (Var, Var) => Binary) = OuterJoin[T, R](underlying, other, func)

  def orderByIndex(index: String): OrderBy[T] = orderByIndex(index: Order)

  def orderByIndex(index: Order): OrderBy[T] = OrderBy[T](underlying, Seq(), Some(index))

  def orderBy(keys: Order*) = OrderBy[T](underlying, keys)

  def withFields(keys: Any*) = WithFields(underlying, keys)


  ///


  // def groupBy(method: AggregateByMethod, attrs: String*) = GroupBy(underlying, method, attrs)


  // add dummy implicit to allow methods for Ref


  def merge[B >: T](other: Sequence[B]) = MergeSequence(underlying, other)

  def foreach(f: Var => Typed) = ForEach(underlying, f)


}


trait Stream[T] extends Sequence[T]


trait Selection[T] extends Typed {


  override val underlying = this


  def update(attributes: Map[String, Any]): Update[T] = update(attributes, UpdateOptions())

  def update(attributes: Map[String, Any], options: UpdateOptions) = Update[T](underlying, FuncWrap(attributes), options)

  def update(p: Predicate1): Update[T] = update(p, UpdateOptions())

  def update(p: Predicate1, options: UpdateOptions) = Update[T](underlying, FuncWrap(p), options)

  def update(d: Document): Update[T] = update((x: Var) => MakeObj2(d))

  def update(d: Document, options: UpdateOptions): Update[T] = Update[T](underlying, FuncWrap(MakeObj2(d)), options)


  def replace(p: Predicate1): Replace[T] = replace(p, UpdateOptions())

  def replace(p: Predicate1, options: UpdateOptions): Replace[T] = Replace(underlying, p, options)

  def replace(d: Document): Replace[T] = replace(d, UpdateOptions())

  def replace(d: Document, options: UpdateOptions): Replace[T] = Replace(underlying, MakeObj2(d), options)

  def replace(data: Map[String, Any]): Replace[T] = replace(data, UpdateOptions())

  def replace(data: Map[String, Any], options: UpdateOptions): Replace[T] = Replace(underlying, FuncWrap(data), options)

  def delete: Delete[T] = delete()

  def delete(durability: Option[Durability.Kind] = None): Delete[T] = Delete(underlying)

}

trait StreamSelection[T] extends Selection[T] with Stream[T] {
  override val underlying = this

  def between(start: Int, stop: Int): Between[T] = between(start, stop, BetweenOptions())

  def between(start: String, stop: String): Between[T] = between(start, stop, BetweenOptions())

  def between(start: Int, stop: Int, options: BetweenOptions) = Between(underlying, start, stop, options)

  def between(start: String, stop: String, options: BetweenOptions) = Between(underlying, start, stop, options)


}

trait SingleSelection[T] extends Selection[T] {
  def merge(value: Any) = new Merge(underlying.asInstanceOf[Typed], Expr(value).asInstanceOf[Typed]) with ProduceAnyDocument
}


trait Filterable[T] extends Typed {

  //def filter(value: Binary): Filter[T] = filter((x: Var) => value)

  override val underlying = this

  def filter(value: Map[String, Any]): Filter[T] = filter(value, false)

  def filter(value: Map[String, Any], default: Boolean): Filter[T] = Filter[T](underlying, FuncWrap(value), default)

  // def filter(value: Typed): Filter[T] = filter(value, false)

  // def filter(value: Typed, default: Boolean): Filter[T] = Filter(this, FuncWrap(value), default)

  def filter(f: Var => Binary): Filter[T] = filter(f, false)

  def filter(f: Var => Binary, default: Boolean): Filter[T] = Filter[T](underlying, FuncWrap(f: ScalaBooleanPredicate1), default)

}
