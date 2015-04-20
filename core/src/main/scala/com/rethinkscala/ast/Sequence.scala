package com.rethinkscala.ast

import com.rethinkscala.magnets.GroupFilterMagnet

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

  def count(): Count = Count(underlying)

  def count(value: String): Count = count(value: Datum)

  def count(value: Double): Count = count(value: Datum)

  def count(value: Boolean): Count = count(value: Datum)

  def count(value: Datum): Count = Count(underlying, value.optWrap)

  def count(f: Var => Binary): Count = Count(underlying, f.optWrap)

  def count(f: japi.BooleanPredicate): Count = Count(underlying, f.optWrap)

  def sum(): Sum[T] = Sum(underlying)

  def sum(value: String): Sum[T] = Sum(underlying, value.optWrap)

  def sum(f: Var => Numeric): Sum[T] = Sum(underlying, f.optWrap)

  def sum(f: japi.NumericPredicate): Sum[T] = Sum(underlying, f.optWrap)

  def group[R](magnet: GroupFilterMagnet[R]): Group[R, T] = Group[R, T](underlying, magnet().map(_.wrap))

  def max(): Max[T] = Max(underlying)

  def max(field: String): Max[T] = Max(underlying, field.optWrap)

  def max(f: Var => Typed): Max[T] = Max(underlying, f.optWrap)

  def max(f: japi.Predicate): Max[T] = Max(underlying, f.optWrap)

  def min(): Min[T] = Min(underlying)

  def min(field: String): Min[T] = Min(underlying, field.optWrap)

  def min(f: japi.Predicate): Min[T] = Min(underlying, f.optWrap)

  def min(f: Var => Typed): Min[T] = Min(underlying, f.optWrap)

  def avg(): Avg[T] = Avg(underlying)

  def avg(field: String): Avg[T] = Avg(underlying, field.optWrap)

  def avg(f: japi.NumericPredicate): Avg[T] = Avg(underlying, f.optWrap)

  def avg(f: Var => Numeric): Avg[T] = Avg(underlying, f.optWrap)

  def contains(field: String, fields: String*): Contains[T] = Contains(underlying, fields.+:(field).map(FuncWrap(_)))

  def contains(field: Double, fields: Double*): Contains[T] = Contains(underlying, fields.+:(field).map(FuncWrap(_)))

  def contains(fields: Datum*): Contains[T] = Contains(underlying, fields.map(FuncWrap(_)))

  def contains(f: japi.BooleanPredicate): Contains[T] = Contains(underlying, Seq(f.wrap))

  def contains(f: Var => Binary): Contains[T] = Contains(underlying, Seq(f.wrap))

  def ?(attr: Datum): Contains[T] = contains(attr)

}

trait IndexTyped[T] extends Typed {

  override val underlying = this

  def apply(index: Int) = Nth(underlying, index)
}

trait Sequence[T, Cursor[_]] extends ArrayTyped[T] with Multiply with Filterable[T, Cursor] with Record with Aggregation[T] with IndexTyped[T] {


//  type CC[A] = AbstractCursor[A]
  type ElementType = T


  override val underlying = this

  def indexesOf[R >: Datum](value: R): ProduceSeq[Long, Cursor] = IndexesOf[T, Cursor](underlying, value.wrap)

  def isEmpty: ProduceBinary = IsEmpty(underlying)

  def sample(amount: Int): ProduceSeq[T, Cursor] = Sample[T, Cursor](underlying, amount)

  def indexesOf(p: Var => Binary): ProduceSeq[Long, Cursor] = IndexesOf[T, Cursor](underlying, p.wrap)

  def skip(amount: Int): ProduceSeq[T, Cursor] = Skip[T, Cursor](underlying, amount)

  def limit(amount: Int): ProduceSeq[T, Cursor] = Limit[T, Cursor](underlying, amount)

  def slice(start: Int = 0, end: Int = -1): ProduceSeq[T, Cursor] = Slice[T, Cursor](underlying, start, end, BoundOptions())

  def slice(start: Int, end: Int, bounds: BoundOptions): ProduceSeq[T, Cursor] = if (end > 0) Slice(underlying, start, end, bounds)
  else
    Slice(underlying, List(start, end).max, -1, BoundOptions(rightBound = Some(Bound.Closed)))


  def apply(range: SliceRange): ProduceSeq[T, Cursor] = slice(range.start, range.end)

  def union[R, CR[_]](sequence: Sequence[R, CR]): ProduceAnySequence = Union(underlying, sequence)

  def eqJoin[R](attr: String, other: Sequence[R, Cursor]): Join[T, R, Cursor] = EqJoin[T, R, Cursor](underlying, Left(attr), other)

  def eqJoin[R](attr: String, other: Sequence[R, Cursor], index: String): Join[T, R, Cursor] = EqJoin[T, R, Cursor](underlying, Left(attr), other, index)

  def eqJoin[R](func: Var => Typed, other: Sequence[R, Cursor], index: String): Join[T, R, Cursor] = EqJoin[T, R, Cursor](underlying, Right(func), other, index)

  def eqJoin[R](func: Var => Typed, other: Sequence[R, Cursor]): Join[T, R, Cursor] = EqJoin[T, R, Cursor](underlying, Right(func), other)

  def innerJoin[R](other: Sequence[R, Cursor], func: (Var, Var) => Binary): Join[T, R, Cursor] = InnerJoin[T, R, Cursor](underlying, other, func)

  def outerJoin[R](other: Sequence[R, Cursor], func: (Var, Var) => Binary): Join[T, R, Cursor] = OuterJoin[T, R, Cursor](underlying, other, func)

  def orderByIndex(index: String): OrderBy[T, Cursor] = orderByIndex(index: Order)

  def orderByIndex(index: Order): OrderBy[T, Cursor] = OrderBy[T, Cursor](underlying, Seq(), Some(index))

  def orderBy(keys: Order*): OrderBy[T, Cursor] = OrderBy[T, Cursor](underlying, keys)

  def withFields(keys: Any*): WithFields[T, Cursor] = WithFields[T, Cursor](underlying, keys)

  def distinct: ProduceSeq[T, Cursor] = Distinct(underlying)

  def merge[B >: T](other: Sequence[B, Cursor]): ProduceSeq[T, Cursor] = MergeSequence(underlying, other)

  def foreach(f: Var => Typed):ForEach[T,Cursor] = ForEach(underlying, f)

}


trait Stream[T, C[_]] extends Sequence[T, C] {
  self: ProduceSequence[T] =>
}


trait Selection[T] extends Typed {

  override val underlying = this

  def update(attributes: Map[String, Any]): Update[T] = update(attributes, UpdateOptions())

  def update(attributes: Map[String, Any], options: UpdateOptions) = Update[T](underlying, attributes.wrap, options)

  def update(p: Var=>Typed): Update[T] = update(p, UpdateOptions())

  def update(p: Var=>Typed, options: UpdateOptions) = Update[T](underlying, p.wrap, options)

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

 // def between(start: Int, stop: Int): Between[T, C] = between(start, stop, BetweenOptions())

  def between(start: Typed, stop: Typed): Between[T, C] = between(start, stop, BetweenOptions())

  //def between(start: Int, stop: Int, options: BetweenOptions) = Between(underlying, start, stop, options)

  def between(start: Typed, stop: Typed, options: BetweenOptions) = Between(underlying, start, stop, options)

}

trait SingleSelection[T] extends Selection[T]

trait Filterable[T, C[_]] extends Typed {

  override val underlying = this

  def filter(value: Map[String, Any]): Filter[T, C] = Filter[T, C](underlying, FuncWrap(value), None)

  def filter(value: Map[String, Any], default: Typed): Filter[T, C] = Filter[T, C](underlying, FuncWrap(value), Some(default))

  def filter(value: ProduceBinary): Filter[T, C] = Filter[T, C](underlying, FuncWrap(value), None)

  def filter(value: ProduceBinary, default: Typed): Filter[T, C] = Filter[T, C](underlying, FuncWrap(value), Some(default))

  def filter(f: Var => Binary): Filter[T, C] = Filter[T, C](underlying, FuncWrap(f: BooleanPredicate1), None)

  def filter(f: Var => Binary, default: Typed): Filter[T, C] = Filter[T, C](underlying, FuncWrap(f: BooleanPredicate1), Some(default))

}
