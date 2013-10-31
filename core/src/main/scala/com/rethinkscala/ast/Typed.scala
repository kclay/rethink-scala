package com.rethinkscala.ast

import com.rethinkscala._
import scala.util.matching.Regex
import com.rethinkscala.net._
import org.joda.time.DateTime
import scala.Some
import com.rethinkscala.BoundOptions
import com.rethinkscala.net.Connection
import com.rethinkscala.UpdateOptions
import com.rethinkscala.JoinResult
import com.rethinkscala.net.BlockingQuery
import com.rethinkscala.utils.Applicator2

trait Produce[ResultType] extends Term {

  type resultType = ResultType


  protected val underlyingTerm: Term = this

  def toQuery[R](implicit c: Connection, tt: Manifest[R]): Query[R] = new BlockingQuery[R](underlyingTerm, c, tt)

  //http://stackoverflow.com/a/3461734
  def run(implicit c: Connection, mf: Manifest[ResultType]): Either[RethinkError, ResultType] = toQuery.toResult

  def as[R <: ResultType](implicit c: Connection, tt: Manifest[R]): Either[RethinkError, R] = toQuery.toResult


  def toOpt(implicit c: Connection, mf: Manifest[ResultType]) = run fold(x => None, Some(_))

  def asOpt[R <: ResultType](implicit c: Connection, tt: Manifest[R], d: DummyImplicit) = as[R] fold(x => None, Some(_))

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


sealed trait Typed extends ImplicitConversions {

  // TODO : Fix me
  def term = this.asInstanceOf[Term]

  val underlying = this

  def info = Info(underlying)

  def typeOf = TypeOf(underlying)

  def coerceTo(dataType: DataType) = CoerceTo(underlying, dataType)
}

trait JoinTyped[L, R] extends Typed {
  override val underlying = this

  def zip = Zip(underlying)
}

trait TableTyped extends Typed


trait Addition extends Typed {

  override val underlying = this

  def +(other: Addition) = add(other)

  def add(other: Addition) = Add(underlying, other)

  def +=(other: Addition) = Add(underlying, other)
}


trait Literal extends Addition {
  def unary_~ = not

  override val underlying = this

  def not = Not(underlying)


  def ===(other: Literal) = eq(other)

  def eq(other: Literal) = Eq(underlying, other)

  def !=(other: Literal) = ne(other)

  def =!=(other: Literal) = ne(other)


  def ne(other: Literal) = Ne(underlying, other)

  def <(other: Literal) = lt(other)

  def lt(other: Literal) = Lt(underlying, other)

  def <=(other: Literal) = lte(other)

  def lte(other: Literal) = Le(underlying, other)

  def >(other: Literal) = gt(other)

  def gt(other: Literal) = Gt(underlying, other)

  def >=(other: Literal) = gte(other)

  def gte(other: Literal) = Ge(underlying, other)


}


trait MapTyped extends Typed

trait Array extends Typed

trait ArrayTyped[T] extends Sequence[T] with Array {


  override val underlying = this

  def append(value: Datum) = Append(underlying, value)

  def :+(value: Datum) = append(value)

  def prepend(value: Datum) = Prepend(underlying, value)

  def +:(value: Datum) = prepend(value)

  def diff(values: Datum*) = Difference(underlying, Expr(values))

  def diff(array: ArrayTyped[_]) = Difference(underlying, array)

  def idiff(array: ArrayTyped[_]) = Difference(array, underlying)

  def idiff(values: Datum*) = Difference(Expr(values), underlying)


  def setInert(value: Datum) = SetInsert(underlying, value)

  def setUnion(values: Datum*) = SetUnion(underlying, values)

  def setIntersection(values: Datum*) = SetIntersection(underlying, values)


  def setDifference(values: Datum*) = SetDifference(underlying, values)

  def insertAt(index: Int, value: Datum) = InsertAt(underlying, index, value)

  def spliceAt(index: Int, values: Datum*) = SpliceAt(underlying, index, values)

  def deleteAt(start: Int, end: Option[Int] = None) = DeleteAt(underlying, start, end)

  def changeAt(index: Int, value: Datum) = ChangeAt(underlying, index, value)

}

trait Stream[T] extends Sequence[T]

trait Selection[T] extends Sequence[T] {

  override val underlying = this

  def update(attributes: Map[String, Any], options: UpdateOptions) = Update(underlying, Left(attributes), options)

  def update(attributes: Map[String, Any]): Update[T] = update(attributes, UpdateOptions())

  def update(p: Var => Typed, options: UpdateOptions) = Update(underlying, Right(p), options)

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

trait Multiply extends Typed {

  override val underlying = this

  def *(other: Numeric): Mul = mul(other)

  def *(other: Double): Mul = mul(other)

  def mul(other: Numeric): Mul = Mul(underlying, other)

  def mul(other: Double): Mul = Mul(underlying, other)
}


trait LiteralSequence[T] extends Sequence[T]


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

  def slice(start: Int = 0, end: Int = -1) = Slice(underlying, start, end)

  def apply(range: SliceRange) = Slice(underlying, range.start, range.end)

  def union(sequence: Sequence[_]) = Union(underlying, sequence)

  def ++(sequence: Sequence[_]) = union(sequence)

  def eqJoin[R](attr: String, other: Sequence[R], index: Option[String] = None) = EqJoin(underlying, attr, other, index)

  def innerJoin[R](other: Sequence[R], func: (Var, Var) => Binary) = InnerJoin(underlying, other, func)

  def outerJoin[R](other: Sequence[R], func: (Var, Var) => Binary) = OuterJoin(underlying, other, func)

  def map[R](func: Produce[R]) = RMap[R](underlying, FuncWrap(func))

  //def map(func: Var => Typed) =
  def map(implicit ev: ToAst[T]) = ev.wrap(RMap[T](underlying, _))


  def reduce(base: T)(implicit ev: ToAst[T]) = ev.apply2(Reduce[T](underlying, _, Some(base)))

  def reduce(implicit ev: ToAst[T]) = ev.apply2(Reduce[T](underlying, _, None))


  /*def reduce(base: Option[Any] = None)(i):

  def reduce(op: (ev.TypeMember, ev.TypeMember) => Typed, base: Option[Any] = None): Reduce[T] = Reduce[T](underlying, op, base)*/


  //Reduce(underlying, toPredicate2(op), base)


  def concatMap(func: Var => Typed) = ConcatMap(underlying, FuncWrap(func))

  def concatMap(func: Typed) = ConcatMap(underlying, FuncWrap(func))

  def order(keys: Ordering*) = OrderBy(underlying, keys)

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


trait Hash {
  self: Typed =>
  type FieldProduce

  override val underlying = this

  def field(name: String): FieldProduce

  def apply[T <: Typed](name: String): T = GetField(underlying, name).asInstanceOf[T]

  def \(name: String) = field(name)
}

trait Record extends Typed with Hash {

  override val underlying = this

  def pluck(attrs: String*) = Pluck(underlying, attrs)

  def pluck(m: Map[String, Any]) = Pluck(underlying, m)

  def without(attrs: String*) = Without(underlying, attrs)


  def merge(other: Record) = Merge(underlying, other)

  def merge(other: Map[String, Any]) = Merge(underlying, other)

  def +(other: Record) = merge(other)

  def hasFields(values: String*) = HasFields(underlying, values)

  def keys = Keys(underlying)

}

trait Binary extends Typed {

  override val underlying = this

  def &(other: Binary) = and(other)

  def and(other: Binary) = All(underlying, other)

  def rand(other: Binary) = All(other, underlying)

  def &>(other: Binary) = rand(other)

  // or
  def ||(other: Binary) = or(other)

  def or(other: Binary) = Or(underlying, other)

  // right or
  def >|(other: Binary) = ror(other)

  def ror(other: Binary) = Or(other, underlying)
}

trait Strings extends Literal {


  override val underlying = this
  //

  // def ===(regexp: String) = find(regexp)
  def find(regexp: Regex): Match = find(regexp.toString())

  def find(regex: String): Match = Match(underlying, regex)
}

trait Numeric extends Literal with Multiply with Binary {


  override val underlying = this

  def -(other: Numeric) = sub(other)

  def -(other: Double) = sub(other)

  def sub(other: Numeric): Sub = Sub(underlying, other)

  def sub(other: Double): Sub = Sub(underlying, other)

  def /(other: Numeric) = div(other)

  def /(other: Double) = div(other)

  def div(other: Numeric): Div = Div(underlying, other)

  def div(other: Double): Div = Div(underlying, other)

  def %(other: Numeric) = mod(other)

  def %(other: Double) = mod(other)

  def mod(other: Numeric) = Mod(underlying, other)

  def mod(other: Double) = Mod(underlying, other)
}

trait Filterable[T] extends Typed {
  self: Sequence[T] =>
  //def filter(value: Binary): Filter[T] = filter((x: Var) => value)

  def filter(value: Map[String, Any]): Filter[T] = filter(value, false)

  def filter(value: Map[String, Any], default: Boolean): Filter[T] = Filter(underlying, FuncWrap(value), default)

  // def filter(value: Typed): Filter[T] = filter(value, false)

  // def filter(value: Typed, default: Boolean): Filter[T] = Filter(this, FuncWrap(value), default)

  def filter(f: Var => Binary): Filter[T] = filter(f, false)

  def filter(f: Var => Binary, default: Boolean): Filter[T] = Filter[T](underlying, FuncWrap(f: BooleanPredicate1), default)

}

trait TimeTyped extends Literal with Produce[DateTime] {
  implicit def dateTimeToTimeTyped(dt: DateTime) = Expr(dt)

  override val underlying = this

  def inTimeZone(timezone: String): InTimeZone = InTimeZone(underlying, Right(timezone))

  def inTimeZone(time: TimeTyped): InTimeZone = InTimeZone(underlying, Left(time))

  def timezone = Timezone(underlying)

  def during(start: DateTime, end: DateTime, bounds: Option[BoundOptions] = None): During = During(underlying, start, end, bounds)

  //def during(start: TimeTyped, end: TimeTyped, bounds: Option[BoundOptions] = None):During = During(underlying, start, end, bounds)
  def date = Date(underlying)

  def day = Day(underlying)

  def timeOfDay = TimeOfDay(underlying)

  def year = Year(underlying)

  def month = Month(underlying)

  def dayOfWeek = DayOfWeek(underlying)

  def dayOfYear = DayOfYear(underlying)

  def hours = Hours(underlying)

  def minutes = Minutes(underlying)

  def seconds = Seconds(underlying)

  def toISO8601 = ToISO8601(underlying)

  def toEpochTime = ToEpochTime(underlying)


  //def inTimeZone(timezone:DateTimeZone)= inTimeZone(timezone.)

}

trait Ref extends ArrayTyped[Any] with Numeric with Binary with Record with Literal with Strings {
  override val underlying = this
}

trait ProduceSequence[T] extends Sequence[T] with Produce[Iterable[T]] {

  type FieldProduce = ProduceTypedArray[T]

  def field(name: String): ProduceTypedArray[T] = GetField[T](this, name)

  def run(implicit c: Connection, mf: Manifest[T], d: DummyImplicit): Either[RethinkError, Seq[T]] = toQuery[T].toResult

  def as[R <: T](implicit c: Connection, mf: Manifest[R], d: DummyImplicit): Either[RethinkError, Seq[R]] = toQuery[R].toResult

  def toOpt(implicit c: Connection, mf: Manifest[T], d: DummyImplicit): Option[Seq[T]] = run fold(x => None, Some(_))


}

trait ProduceAnySequence extends ProduceSequence[Any]

trait ProduceSet extends ProduceArray

trait ProduceBinary extends Produce[Boolean] with Binary

//trait ProduceLiteral extends ProduceLiteral with Literal


trait ProduceDocument[T <: Document] extends Produce[T] with Record with DocumentConversion[T] {

  type FieldProduce = ProduceAny

  def field(name: String): ProduceAny = GetField(this, name)
}

trait ProduceAnyDocument extends ProduceDocument[Document] with Record

trait ProduceNumeric extends Produce[Double] with Numeric

trait ProduceString extends Produce[String] with Strings

trait ForwardTyped {
  self: Produce[_] with Typed =>
  override lazy val args = underlyingTerm.args
  override lazy val optargs = underlyingTerm.optargs

  override def ast = underlyingTerm.ast

  override protected val underlyingTerm: Term = underlying.asInstanceOf[Term]

  def termType = underlying.term
}

trait ProduceAny extends Produce[Any] with Ref {


  // def numeric: ProduceNumeric =
  private[this] val any = this

  def numeric = new ProduceNumeric {
    override val underlying = any
    override lazy val args = underlyingTerm.args
    override lazy val optargs = underlyingTerm.optargs

    override def ast = underlyingTerm.ast

    override protected val underlyingTerm: Term = any

    def termType = underlyingTerm.termType
  }

  def string: ProduceString = new ProduceString {
    override val underlying = any
    override lazy val args = underlyingTerm.args
    override lazy val optargs = underlyingTerm.optargs

    override def ast = underlyingTerm.ast

    override protected val underlyingTerm: Term = any

    def termType = underlyingTerm.termType
  }


  def record: Record = this

  def array[T]: ArrayTyped[T] = this.asInstanceOf[ArrayTyped[T]]


  type FieldProduce = ProduceAny

  def field(name: String) = GetField(this.asInstanceOf[Typed], name)
}

trait ProduceSingleSelection extends ProduceAnyDocument with SingleSelection[Any]

trait ProduceTypedSingleSelection[T <: Document] extends SingleSelection[T] with ProduceDocument[T]

trait ProduceStreamSelection extends ProduceAnySequence with StreamSelection[Any]

trait ProduceTypedStreamSelection[T] extends ProduceSequence[T] with StreamSelection[T]

trait ProduceArray extends ProduceAnySequence with ArrayTyped[Any]

trait ProduceTypedArray[T] extends ProduceSequence[T] with ArrayTyped[T]

trait ProduceJoin[L, R] extends ProduceSequence[JoinResult[L, R]] with JoinTyped[L, R] {
  override val underlying = this
}

trait ProduceTime extends TimeTyped

sealed trait LogicSignature
