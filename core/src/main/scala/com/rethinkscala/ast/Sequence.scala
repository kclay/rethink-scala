package com.rethinkscala.ast

import com.rethinkscala.magnets.GroupFilterMagnet
import com.rethinkscala.net.AbstractCursor
import com.rethinkscala.{BoundOptions, _}

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 2/24/14
 * Time: 8:15 AM 
 */


object Sequence {
  implicit def mapDocumentToSequence[T <: Document, ST, C[_], S[ST] <: Sequence[ST, C]] = CanMap[T, S[ST], ST]

  implicit def docToFunctional[T <: Document, C[_]](seq: Sequence[T, C]) = new ToFunctional[T, Var, C](seq)

  implicit def toFunctional[T, C[_]](seq: Sequence[T, C])(implicit ast: ToAst[T]): ToFunctional[T, ast.TypeMember, C] = new ToFunctional[T, ast.TypeMember, C](seq)


  implicit class ScalaSequence[T, C[_]](underlying: Sequence[T, C]) {

    def ++[R, CR[_]](sequence: Sequence[R, CR]) = underlying.union(sequence)

  }

}

trait Aggregation[T] {

  // self:ProduceSequence[T]=>
  val underlying = this

  def count() = Count(underlying)

  def count(value: String): Count = count(value: Datum)

  def count(value: Double): Count = count(value: Datum)

  def count(value: Boolean): Count = count(value: Datum)

  def count(value: Datum) = Count(underlying, value.optWrap)

  def count(f: Var => Binary) = Count(underlying, f.optWrap)

  def count(f: japi.BooleanPredicate) = Count(underlying, f.optWrap)

  def sum() = Sum(underlying)

  def sum(value: String) = Sum(underlying, value.optWrap)

  def sum(f: Var => Numeric) = Sum(underlying, f.optWrap)

  def sum(f: japi.NumericPredicate) = Sum(underlying, f.optWrap)

  def group[R](magnet: GroupFilterMagnet[R]): Group[R, T] = Group[R, T](underlying, magnet().map(_.wrap))

  def max() = Max(underlying)

  def max(field: String) = Max(underlying, field.optWrap)

  def max(f: Var => Typed) = Max(underlying, f.optWrap)

  def max(f: japi.Predicate) = Max(underlying, f.optWrap)

  def min() = Max(underlying)

  def min(field: String) = Min(underlying, field.optWrap)

  def min(f: japi.Predicate) = Min(underlying, f.optWrap)

  def min(f: Var => Typed) = Min(underlying, f.optWrap)

  def avg() = Avg(underlying)

  def avg(field: String) = Avg(underlying, field.optWrap)

  def avg(f: japi.NumericPredicate) = Avg(underlying, f.optWrap)

  def avg(f: Var => Numeric) = Avg(underlying, f.optWrap)

  def contains(field: String, fields: String*) = Contains(underlying, fields.+:(field).map(FuncWrap(_)))

  def contains(field: Double, fields: Double*) = Contains(underlying, fields.+:(field).map(FuncWrap(_)))

  def contains(fields: Datum*) = Contains(underlying, fields.map(FuncWrap(_)))

  def contains(f: japi.BooleanPredicate) = Contains(underlying, Seq(f.wrap))

  def contains(f: Var => Binary) = Contains(underlying, Seq(f.wrap))

  def ?(attr: Datum) = contains(attr)

}

trait IndexTyped[T] extends Typed {

  override val underlying = this

  def apply(index: Int) = Nth(underlying, index)
}

trait Sequence[T, Cursor[_]] extends ArrayTyped[T] with Multiply with Filterable[T, Cursor] with Record with Aggregation[T] with IndexTyped[T] {


  type CC[A] = AbstractCursor[A]
  type ElementType = T


  override val underlying = this

  def indexesOf[R >: Datum](value: R) = IndexesOf[T, Cursor](underlying, value.wrap)

  def isEmpty = IsEmpty(underlying)

  def sample(amount: Int) = Sample[T, Cursor](underlying, amount)

  def indexesOf(p: Var => Binary) = IndexesOf[T, Cursor](underlying, p.wrap)

  def skip(amount: Int) = Skip[T, Cursor](underlying, amount)

  def limit(amount: Int) = Limit[T, Cursor](underlying, amount)

  def slice(start: Int = 0, end: Int = -1) = Slice[T, Cursor](underlying, start, end, BoundOptions())

  def slice(start: Int, end: Int, bounds: BoundOptions) = if (end > 0) Slice(underlying, start, end, bounds)
  else
    Slice(underlying, List(start, end).max, -1, BoundOptions(rightBound = Some(Bound.Closed)))


  def apply(range: SliceRange) = slice(range.start, range.end)

  def union[R, CR[_]](sequence: Sequence[R, CR]) = Union(underlying, sequence)

  def eqJoin[R](attr: String, other: Sequence[R, Cursor]) = EqJoin[T, R, Cursor](underlying, Left(attr), other)

  def eqJoin[R](attr: String, other: Sequence[R, Cursor], index: String) = EqJoin[T, R, Cursor](underlying, Left(attr), other, index)

  def eqJoin[R](func: Var => Typed, other: Sequence[R, Cursor], index: String) = EqJoin[T, R, Cursor](underlying, Right(func), other, index)

  def eqJoin[R](func: Var => Typed, other: Sequence[R, Cursor]) = EqJoin[T, R, Cursor](underlying, Right(func), other)

  def innerJoin[R](other: Sequence[R, Cursor], func: (Var, Var) => Binary) = InnerJoin[T, R, Cursor](underlying, other, func)

  def outerJoin[R](other: Sequence[R, Cursor], func: (Var, Var) => Binary) = OuterJoin[T, R, Cursor](underlying, other, func)

  def orderByIndex(index: String): OrderBy[T, Cursor] = orderByIndex(index: Order)

  def orderByIndex(index: Order): OrderBy[T, Cursor] = OrderBy[T, Cursor](underlying, Seq(), Some(index))

  def orderBy(keys: Order*) = OrderBy[T, Cursor](underlying, keys)

  def withFields(keys: Any*) = WithFields[T, Cursor](underlying, keys)

  def distinct = Distinct(underlying)

  def merge[B >: T](other: Sequence[B, Cursor]) = MergeSequence(underlying, other)

  def foreach(f: Var => Typed) = ForEach(underlying, f)

}


trait Stream[T, C[_]] extends Sequence[T, C] {
  self: ProduceSequence[T] =>
}


trait Selection[T] extends Typed {

  override val underlying = this

  def update(attributes: Map[String, Any]): Update[T] = update(attributes, UpdateOptions())

  def update(attributes: Map[String, Any], options: UpdateOptions) = Update[T](underlying, attributes.wrap, options)

  def update(p: Predicate1): Update[T] = update(p, UpdateOptions())

  def update(p: Predicate1, options: UpdateOptions) = Update[T](underlying, p.wrap, options)

  def update(d: Document): Update[T] = update((x: Var) => MakeObj2(d))

  def update(d: Document, options: UpdateOptions): Update[T] = Update[T](underlying, MakeObj2(d).wrap, options)

  def replace(p: Var => Typed): Replace[T] = replace(p, UpdateOptions())

  def replace(p: Var => Typed, options: UpdateOptions): Replace[T] = Replace(underlying, (p: Predicate1).wrap, options)

  def replace[R <: Document](d: R): Replace[R] = replace(d, UpdateOptions())

  def replace[R <: Document](d: R, options: UpdateOptions): Replace[R] = Replace[R](underlying, MakeObj2(d).wrap, options)

  def replace(data: Map[String, Any]): Replace[T] = replace(data, UpdateOptions())

  def replace(data: Map[String, Any], options: UpdateOptions): Replace[T] = Replace[T](underlying, data.wrap, options)

  def delete: Delete[T] = delete()

  def delete(durability: Option[Durability.Kind] = None): Delete[T] = Delete(underlying)

}

trait StreamSelection[T, C[_]] extends Selection[T] with Stream[T, C] {
  self: ProduceSequence[T] =>
  override val underlying = this

  def between(start: Int, stop: Int): Between[T, C] = between(start, stop, BetweenOptions())

  def between(start: String, stop: String): Between[T, C] = between(start, stop, BetweenOptions())

  def between(start: Int, stop: Int, options: BetweenOptions) = Between(underlying, start, stop, options)

  def between(start: String, stop: String, options: BetweenOptions) = Between(underlying, start, stop, options)

}

trait SingleSelection[T] extends Selection[T]

trait Filterable[T, C[_]] extends Typed {

  override val underlying = this

  def filter(value: Map[String, Any]): Filter[T, C] = Filter[T, C](underlying, FuncWrap(value), None)

  def filter(value: Map[String, Any], default: Typed): Filter[T, C] = Filter[T, C](underlying, FuncWrap(value), Some(default))

  def filter(value: ProduceBinary): Filter[T, C] = Filter[T, C](underlying, FuncWrap(value), None)

  def filter(value: ProduceBinary, default: Typed): Filter[T, C] = Filter[T, C](underlying, FuncWrap(value), Some(default))

  def filter(f: Var => Binary): Filter[T, C] = Filter[T, C](underlying, FuncWrap(f: ScalaBooleanPredicate1), None)

  def filter(f: Var => Binary, default: Typed): Filter[T, C] = Filter[T, C](underlying, FuncWrap(f: ScalaBooleanPredicate1), Some(default))

}
