package com.rethinkdb.ast

import com.rethinkdb._
import scala.util.matching.Regex
import scala.Some
import com.rethinkdb.BlockingQuery
import scala.reflect.ClassTag


trait Produce[ResultType] extends Term {
  import scala.reflect.runtime.universe._

  def toQuery[R](implicit c: Connection, tt: TypeTag[R]): Query[R] = new BlockingQuery[R](this, c,tt)

  //http://stackoverflow.com/a/3461734
  //def run(implicit c: Connection, mf: Manifest[ResultType]): Either[RethinkError, Option[ResultType]] = toQuery.toResult

  def run[R <: ResultType](implicit c: Connection, tt:TypeTag[R]):Either[RethinkError,R] = toQuery.toResult

}
/*
sealed trait DataType{
  def name
}

case object ObjectData extends DataType{
  def name="object"
}
case object StringData extends DataType{
  def name="string"
}
case object ArrayData extends DataType{
  def name ="array"
}
 */
sealed trait Typed {
  implicit def toPredicate1(f: (Var) => Typed) = new Predicate1(f)
  implicit def toBooleanPredicate1(f: (Var) => Binary) = new BooleanPredicate1(f)

  def info = Info(this)
  def typeOf = TypeOf(this)
}

trait Convertable extends Typed {
  //def coerceTo(dataType:DataType)
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

trait ArrayTyped extends Typed

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

trait Appendable extends Typed {
  def append(value: Typed) = Append(this, value)
  def :+(value: Typed) = append(value)
}

trait Countable extends Typed {

  def size = count
  def count = Count(this)
  def count(filter: BooleanPredicate) = Count(this, Some(filter))
  def count(value: Binary) = Count(this, Some((x: Var) => value))
}
trait Sequence extends Addition with Multiply with Functional with Appendable {

  //def coerceTo(dataType: DataType)=CoerceTo(this,dataType)

  def indexesOf(value: Datum): IndexesOf = IndexesOf(this, Left(value))
  def indexesOf(value: Binary): IndexesOf = indexesOf((x: Var) => value)

  def isEmpty = IsEmpty(this)

  def sample(amount: Int) = Sample(this, amount)
  def indexesOf(predicate: BooleanPredicate1) = IndexesOf(this, Right(predicate))
  def prepend(value: Typed) = Prepend(this, value)
  def +:(value: Typed) = prepend(value)
  def apply(index: Int) = Nth(this, index)

  def skip(amount: Int) = Skip(this, amount)

  def slice(start: Int = 0, end: Int = -1) = Slice(this, start, end)

  def apply(prange: SliceRange) = Slice(this, prange.start, prange.end)

  def union(sequences: Sequence) = Union(this, sequences)
}

trait Functional extends Typed {
  def map(func: Predicate1) = RMap(this, func)

  def concatMap(func: Predicate1) = ConcatMap(this, func)
}
trait Json extends Functional with Appendable {

  def pluck(attrs: String*) = Pluck(this, attrs)

  def without(attrs: String*) = Without(this, attrs)

  def order(keys: Ordering*) = OrderBy(this, keys)

  def attr(name: String) = GetAttr(this, name)

  def \(name: String) = attr(name)

  def merge(other: Json) = Merge(this, other)

  def +(other: Json) = merge(other)

  def contains(attr: String) = Contains(this, attr)
  def ?(attr: String) = contains(attr)
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

  def filter(value: Binary): Func = filter((x: Var) => value)
  def filter(value: Map[String, Any]): Func = filter((x: Var) => Expr(value))
  def filter(f: Predicate): Func = Func(f)
}

trait Ref extends Numeric with Binary with Json with Sequence with Comparable with Literal with Strings

trait ProduceSequence extends Produce[Iterable[Any]] with Sequence

trait ProduceSet extends ProduceSequence

trait ProduceBinary extends Produce[Boolean] with Binary

//trait ProduceLiteral extends ProduceComparable with Literal

trait ProduceDocument extends Produce[Document]

trait ProduceNumeric extends Produce[Double] with Numeric

trait ProduceString extends Produce[String] with Strings

trait ProduceAny extends Produce[Any] with Ref

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
