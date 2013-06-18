package com.rethinkscala.ast

import com.rethinkscala._
import scala.util.matching.Regex
import scala.Some
import com.rethinkscala.BlockingQuery
import com.rethinkscala.Implicits._

trait Produce[ResultType] extends Term {

  def toQuery[R](implicit c: Connection, tt: Manifest[R]): Query[R] = new BlockingQuery[R](this, c, tt)

  //http://stackoverflow.com/a/3461734
  def run(implicit c: Connection, mf: Manifest[ResultType]): Either[RethinkError, Option[ResultType]] = toQuery.toResult

  def as[R <: ResultType](implicit c: Connection, tt: Manifest[R]): Either[RethinkError, R] = toQuery.toResult

}

sealed trait DataType {
  def name: String
}

case object ObjectData extends DataType {
  def name = "object"
}
case object StringData extends DataType {
  def name = "string"
}
case object ArrayData extends DataType {
  def name = "array"
}

sealed trait Typed {

  implicit def toPredicate1(f: (Var) => Typed) = new Predicate1(f)

  implicit def toBooleanPredicate1(f: (Var) => Binary) = new BooleanPredicate1(f)

  def info = Info(this)

  def typeOf = TypeOf(this)
  def coerceTo(dataType: DataType) = CoerceTo(this, dataType)
}

trait Addition extends Typed {
  def +(other: Addition) = add(other)

  def add(other: Addition) = Add(this, other)

  def +=(other: Addition) = Add(this, other)
}

trait Literal extends Comparable with Addition {
  def ~(other: Term) = not(other)

  def not(other: Term) = Not(this)
}

trait MapTyped extends Typed

trait ArrayTyped extends Sequence {
  def append(value: Datum) = Append(this, value)

  def :+(value: Datum) = append(value)
  def prepend(value: Datum) = Prepend(this, value)

  def +:(value: Datum) = prepend(value)
  def difference(values: Datum*) = Difference(this, values)

  def setInert(value: Datum) = SetInsert(this, value)
  def setUnion(values: Datum*) = SetUnion(this, values)
  def setIntersection(values: Datum*) = SetIntersection(this, values)
  def setDifference(values: Datum*) = SetDifference(this, values)
  def insertAt(index: Int, value: Datum) = InsertAt(this, index, value)
  def spliceAt(index: Int, values: Datum*) = SpliceAt(this, index, values)

  def deleteAt(start: Int, end: Option[Int] = None) = DeleteAt(this, start, end)
  def changeAt(index: Int, value: Datum) = ChangeAt(this, index, value)

}

trait Stream extends Sequence

trait Selection extends Typed

trait StreamSelection extends Selection with Stream {

  def between(start: Int, stop: Int) = Between(this, start, stop)

  def between(start: String, stop: String) = Between(this, start, stop)
}

trait SingleSelection extends Selection

trait Comparable extends Typed {

  def ==(other: Comparable) = eq(other)

  def eq(other: Comparable) = Eq(this, other)

  def !=(other: Comparable) = ne(other)

  def ne(other: Comparable) = Ne(this, other)

  def <(other: Comparable) = lt(other)

  def lt(other: Comparable) = Lt(this, other)

  def <=(other: Comparable) = lte(other)

  def lte(other: Comparable) = Le(this, other)

  def >(other: Comparable) = gt(other)

  def gt(other: Comparable) = Gt(this, other)

  def >=(other: Comparable) = gte(other)

  def gte(other: Comparable) = Ge(this, other)

}

trait Multiply {

  def *(other: Numeric) = mul(other)

  def mul(other: Numeric) = Mul(this, other)
}

trait Sequence extends Multiply with Filterable with Manipulation {

  type ManipulationType = Sequence
  //def coerceTo(dataType: DataType)=CoerceTo(this,dataType)

  def indexesOf(value: Datum): IndexesOf = IndexesOf(this, Left(value))

  //def indexesOf(value: Binary): IndexesOf = indexesOf((x: Var) => value)

  def isEmpty = IsEmpty(this)

  def sample(amount: Int) = Sample(this, amount)

  def indexesOf(predicate: BooleanPredicate1) = IndexesOf(this, Right(predicate))

  def apply(index: Int) = Nth(this, index)

  def skip(amount: Int) = Skip(this, amount)

  def slice(start: Int = 0, end: Int = -1) = Slice(this, start, end)

  def apply(prange: SliceRange) = Slice(this, prange.start, prange.end)

  def union(sequence: Sequence) = Union(this, sequence)

  def ++(sequence: Sequence) = union(sequence)

  def eqJoin(attr: String, other: Sequence, index: Option[String] = None) = EqJoin(this, attr, other, index)

  def innerJoin(other: Sequence, func: BooleanPredicate2) = InnerJoin(this, other, func)

  def outerJoin(other: Table, func: BooleanPredicate2) = OuterJoin(this, other, func)

  def map(func: Predicate1) = RMap(this, func)

  def concatMap(func: Predicate1) = ConcatMap(this, func)

  def order(keys: Ordering*) = OrderBy(this, keys)

  def withFields(keys: String*) = WithFields(this, keys)

  def size = count

  def count = Count(this)

  def count(value: String) = Count(this, Some(Left(value)))

  def count(filter: BooleanPredicate) = Count(this, Some(Right(filter)))

  def count(value: Binary) = Count(this, Some(Right((x: Var) => value)))

  def mapReduce(grouping: Predicate1, mapping: Predicate1,
                reduction: Predicate2, base: Option[Datum] = None) = GroupMapReduce(this, grouping, mapping, reduction, base)

  def groupBy(method: AggregateByMethod, attrs: String*) = GroupBy(this, method, attrs)

  def contains(attrs: Datum*) = Contains(this, attrs)

  def ?(attr: Datum) = contains(attr)
  def pluck(attrs: String*) = Pluck(this, attrs)
  def without(attrs: String*) = Without(this, attrs)

  def merge(other: Sequence) = Merge(this, other)

  def +(other: Sequence) = merge(other)
  def foreach(value: Predicate1) = ForEach(this, value)

}

trait Manipulation {
  type ManipulationType
  def pluck(attrs: String*)
  def without(attrs: String*)
  def merge(other: ManipulationType)

  def +(other: ManipulationType)
}

trait Json extends Typed with Manipulation {

  type ManipulationType = Json
  def pluck(attrs: String*) = Pluck(this, attrs)

  def without(attrs: String*) = Without(this, attrs)

  def attr(name: String) = GetAttr(this, name)

  def \(name: String) = attr(name)

  def merge(other: Json) = Merge(this, other)

  def +(other: Json) = merge(other)
  def hasFields(values: String*) = HasFields(this, values)

  def keys = Keys(this)

}

trait Binary extends Typed {

  def &(other: Binary) = and(other)

  def and(other: Binary) = All(this, other)

  def rand(other: Binary) = All(other, this)

  def &>(other: Binary) = rand(other)

  // or
  def |(other: Binary) = or(other)

  def or(other: Binary) = Or(this, other)

  // right or
  def >|(other: Binary) = ror(other)

  def ror(other: Binary) = Or(other, this)
}

trait Strings extends Literal {

  def ===(regexp: Regex) = find(regexp.toString())

  def ===(regexp: String) = find(regexp)

  def find(regex: String) = Match(this, regex)
}

trait Numeric extends Literal with Multiply with Binary {

  def -(other: Numeric) = sub(other)

  def sub(other: Numeric) = Sub(this, other)

  def /(other: Numeric) = div(other)

  def div(other: Numeric) = Div(this, other)

  def %(other: Numeric) = mod(other)

  def mod(other: Numeric) = Mod(this, other)
}

trait Filterable extends Typed {
  self: Sequence =>
  def filter(value: Binary): Filterable = filter((x: Var) => value)

  def filter(value: Map[String, Any]): Filter = Filter(this, Left(Expr(value)))

  def filter(f: Predicate1): Filter = Filter(this, Right(f))

}

trait Ref extends Numeric with Binary with Json with ArrayTyped with Comparable with Literal with Strings

trait ProduceSequence extends Produce[Iterable[Any]] with Sequence

trait ProduceSet extends ProduceArray

trait ProduceBinary extends Produce[Boolean] with Binary

//trait ProduceLiteral extends ProduceComparable with Literal

trait ProduceDocument extends Produce[Document] with Json

trait ProduceNumeric extends Produce[Double] with Numeric

trait ProduceString extends Produce[String] with Strings

trait ProduceAny extends Produce[Any] with Ref

trait ProduceSelection extends Selection

trait ProduceSingleSelection extends ProduceDocument with ProduceSelection with SingleSelection

trait ProduceStreamSelection extends ProduceSequence with ProduceSelection with StreamSelection

trait ProduceArray extends ProduceSequence with ArrayTyped

sealed trait LogicSignature
