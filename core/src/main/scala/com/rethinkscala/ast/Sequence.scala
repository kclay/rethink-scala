package com.rethinkscala.ast

import com.rethinkscala._
import com.rethinkscala.BoundOptions
import scala.Some

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 2/24/14
 * Time: 8:15 AM 
 */


trait Sequence[T] extends Multiply with Filterable[T] with Record {


  override val underlying = this

  // def field(name: String)(implicit d:DummyImplicit) = GetField(this, name)

  //def \(name: String)(implicit d:DummyImplicit) = field(name)

  //def coerceTo(dataType: DataType)=CoerceTo(this,dataType)

  //def field(name: String) = GetField(this, name)

  //def \(name: String) = field(name)


  def indexesOf(value: Datum) = IndexesOf(underlying, Left(value))

  //def indexesOf(value: Binary): IndexesOf = indexesOf((x: Var) => value)

  def isEmpty = IsEmpty(underlying)

  def sample(amount: Int) = Sample(underlying, amount)

  def indexesOf(p: Var => Binary) = IndexesOf(underlying, Right(p))

  def apply(index: Int) = Nth(underlying, index)

  def skip(amount: Int) = Skip(underlying, amount)

  def limit(amount: Int) = Limit(underlying, amount)

  def slice(start: Int = 0, end: Int = -1) = Slice(underlying, start, end, BoundOptions())

  def slice(start: Int, end: Int, bounds: BoundOptions) = if (end > 0) Slice(underlying, start, end, bounds)
  else
    Slice(underlying, List(start, end).max, -1, BoundOptions(rightBound = Some(Bound.Closed)))


  def apply(range: SliceRange) = slice(range.start, range.end)

  def union(sequence: Sequence[_]) = Union(underlying, sequence)

  def ++(sequence: Sequence[_]) = union(sequence)

  def eqJoin[R](attr: String, other: Sequence[R], index: Option[String] = None) = EqJoin(underlying, attr, other, index)

  def innerJoin[R](other: Sequence[R], func: (Var, Var) => Binary) = InnerJoin[T, R](underlying, other, func)

  def outerJoin[R](other: Sequence[R], func: (Var, Var) => Binary) = OuterJoin[T, R](underlying, other, func)

  //def map[R](func: Produce[R]) = RMap[R](underlying, FuncWrap(func))

  // def map(func:MappingFunction[T])

  //def map(func: Var => Typed) =

  // def map(implicit ev: ToAst[T]) = ev.wrap(RMap[T](underlying, _))


  // def reduce(base: T)(implicit ev: ToAst[T]) = ev.apply2(Reduce[T](underlying, _, Some(base)))

 // def reduce(implicit ev: ToAst[T]) = ev.apply2(Reduce[T](underlying, _, None))

  //def reduce(f: (B, B) => B)(implicit ev: ToAst[T, B]) = ???


  //def reduce0[A1](op: (A1, A1) => Typed)(implicit ev: To[T, A1]) = ???

  /*def reduce(base: Option[Any] = None)(i):

  def reduce(op: (ev.TypeMember, ev.TypeMember) => Typed, base: Option[Any] = None): Reduce[T] = Reduce[T](underlying, op, base)*/


  //Reduce(underlying, toPredicate2(op), base)


  def concatMap(func: Var => Typed) = ConcatMap(underlying, FuncWrap(func))

  def concatMap(func: Typed) = ConcatMap(underlying, FuncWrap(func))

  // TODO : Add function support
  def orderBy(keys: Order*) = OrderBy(underlying, keys)

  def withFields(keys: String*) = WithFields(underlying, keys)

  def size = count

  def count = Count(underlying)

  def count(value: String) = Count(underlying, Some(Left(value)))

  def count(filter: Var => Binary) = Count(underlying, Some(Right(filter)))

  def count(value: Binary) = Count(underlying, Some(Right((x: Var) => value)))

  def mapReduce(grouping: Predicate1, mapping: Predicate1,
                reduction: Predicate2, base: Option[Datum] = None) = GroupMapReduce(underlying, grouping, mapping, reduction, base)

  def groupBy(method: AggregateByMethod, attrs: String*) = GroupBy(underlying, method, attrs)


  //def contains(attrs: Datum*) = Contains(underlying, attrs)

  def contains[T <: DatumOrFunction](attrs: T*) = Contains(underlying, attrs)

  def ?(attr: Datum) = contains(attr)

  // add dummy implicit to allow methods for Ref
  def pluck(attrs: String*)(implicit d: DummyImplicit) = Pluck(underlying, attrs)

  def without(attrs: String*)(implicit d: DummyImplicit) = Without(underlying, attrs)

  def pluck(m: Map[String, Any])(implicit d: DummyImplicit) = Pluck(underlying, m)

  def merge(other: Sequence[_]) = Merge(underlying, other)

  def +(other: Sequence[_]) = merge(other)

  def foreach(f: Var => Typed) = ForEach(underlying, f)

}


trait Stream[T] extends Sequence[T]

trait Selection[T] extends Sequence[T] {

  override val underlying = this

  def update(attributes: Map[String, Any], options: UpdateOptions) = Update[T](underlying, Left(attributes), options)

  def update(attributes: Map[String, Any]): Update[T] = update(attributes, UpdateOptions())

  def update(p: Var => Typed, options: UpdateOptions) = Update[T](underlying, Right(p), options)

  def update(d: Document): Update[T] = update((x: Var) => MakeObj2(d))

  def update(t: Typed, options: UpdateOptions): Update[T] = Update(underlying, Left(t), options)

  def update(t: Typed): Update[T] = update(t, UpdateOptions())

  def update(p: Var => Typed): Update[T] = update(p, UpdateOptions())

  def replace(p: Var => Typed): Replace[T] = Replace(underlying, Right(p), UpdateOptions())

  def replace(d: Document): Replace[T] = replace((x: Var) => MakeObj2(d))

  def replace(data: Map[String, Any]): Replace[T] = Replace(underlying, Left(data), UpdateOptions())

  def delete: Delete[T] = delete()

  def delete(durability: Option[Durability.Kind] = None): Delete[T] = Delete(underlying)

}

trait StreamSelection[T] extends Selection[T] with Stream[T] {
  override val underlying = this

  def between(start: Int, stop: Int) = Between(underlying, start, stop)

  def between(start: String, stop: String) = Between(underlying, start, stop)


}

trait SingleSelection[T] extends Selection[T]


trait Filterable[T] extends Typed {
  self: Sequence[T] =>
  //def filter(value: Binary): Filter[T] = filter((x: Var) => value)

  def filter(value: Map[String, Any]): Filter[T] = filter(value, false)

  def filter(value: Map[String, Any], default: Boolean): Filter[T] = Filter[T](underlying, FuncWrap(value), default)

  // def filter(value: Typed): Filter[T] = filter(value, false)

  // def filter(value: Typed, default: Boolean): Filter[T] = Filter(this, FuncWrap(value), default)

  def filter(f: Var => Binary): Filter[T] = filter(f, false)

  def filter(f: Var => Binary, default: Boolean): Filter[T] = Filter[T](underlying, FuncWrap(f: BooleanPredicate1), default)

}
