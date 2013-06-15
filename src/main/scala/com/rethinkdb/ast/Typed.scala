package com.rethinkdb.ast

import com.rethinkdb._
import scala.util.matching.Regex
import scala.reflect.ClassTag
import com.rethinkdb.ast.IndexesOf
import com.rethinkdb.ast.Mod
import com.rethinkdb.ast.Prepend
import com.rethinkdb.ast.Match
import com.rethinkdb.ast.OrderBy
import com.rethinkdb.ast.Mul
import com.rethinkdb.ast.Predicate2
import scala.Some
import com.rethinkdb.ast.OuterJoin
import com.rethinkdb.ast.EqJoin
import com.rethinkdb.ast.All
import com.rethinkdb.ast.TypeOf
import com.rethinkdb.ast.Pluck
import com.rethinkdb.ast.Contains
import com.rethinkdb.ast.Sub
import com.rethinkdb.ast.Ge
import com.rethinkdb.ast.BooleanPredicate1
import com.rethinkdb.ast.Eq
import com.rethinkdb.ast.InnerJoin
import com.rethinkdb.ast.GetAttr
import com.rethinkdb.ast.Count
import com.rethinkdb.ast.Gt
import com.rethinkdb.ast.Or
import com.rethinkdb.ast.SliceRange
import com.rethinkdb.ast.Le
import com.rethinkdb.ast.IsEmpty
import com.rethinkdb.ast.Var
import com.rethinkdb.ast.Not
import com.rethinkdb.ast.Without
import com.rethinkdb.ast.ConcatMap
import com.rethinkdb.ast.RMap
import com.rethinkdb.ast.Append
import com.rethinkdb.ast.Lt
import com.rethinkdb.ast.Sample
import com.rethinkdb.ast.Div
import com.rethinkdb.ast.Union
import com.rethinkdb.ast.Add
import com.rethinkdb.ast.Table
import com.rethinkdb.ast.Predicate1
import com.rethinkdb.BlockingQuery
import com.rethinkdb.ast.Nth
import com.rethinkdb.ast.Skip
import com.rethinkdb.ast.Slice
import com.rethinkdb.ast.Func
import com.rethinkdb.ast.Merge
import com.rethinkdb.ast.Ne


trait Produce[ResultType] extends Term{




  import scala.reflect.runtime.universe._



  def toQuery[R](implicit c:Connection):Query[R] =new BlockingQuery[R](this,c)

  //http://stackoverflow.com/a/3461734
  def run(implicit c:Connection):Either[RethinkError,Option[ResultType]] = toQuery.toResult

  def run[R](implicit c:Connection,s:DummyImplicit) = toQuery.toResult


  def withResult(result:Any): Option[ResultType] =
    if (withMapProduce) extract[Map[String, Any]](result, defaultValue)(fromMap _)
    else Option(wrap(result))

  val withMapProduce = false

  def wrap[T<:ResultType](value: Any): ResultType=value.asInstanceOf[ResultType]


  def defaultValue:ResultType

  protected def fromMap(m: Map[String, Any]): ResultType = defaultValue

  protected def extract[T: TypeTag](result: Any, defaultValue: ResultType)(f: T => Any): Option[ResultType] = {
   val v =  result match {
      case Some(r:Any)=>wrap(f(r.asInstanceOf[T]))
      case _=>defaultValue

  }
    Option(v)
  }
 /*
  def toNative
  def toResult[ResultType](value:Any)=withResult[]
  def withResult[A: TypeTag](result: A): ResultType =
    if (withMapProduce) extract[A, Map[String, Any],ResultType](result, defaultValue)(fromMap _)
    else wrap(result)

  val withMapProduce = false

  def wrap[ResultType](value: Any): ResultType


  def defaultValue[ResultType]: ResultType

  protected def fromMap[ResultType](m: Map[String, Any]): ResultType = defaultValue

  protected def extract[A: TypeTag, T: TypeTag,ResultType](result: A, defaultValue: ResultType)(f: T => Any): ResultType = {
    typeOf[A] match {
      case t if t =:= typeOf[T] => wrap(f(result.asInstanceOf[T]))
      case _ => defaultValue
    }
  }*/


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
sealed trait Typed{
  implicit def toPredicate1(f: (Var) => Typed) = new Predicate1(f)
  implicit def toBooleanPredicate1(f: (Var) => Binary) = new BooleanPredicate1(f)

  def typeOf=TypeOf(this)
}

trait Convertable extends Typed{
   //def coerceTo(dataType:DataType)
}
trait Addition extends Typed{
  def +(other: Addition) = add(other)

  def add(other: Addition) = Add(this, other)

  def +=(other: Addition) = Add(this, other)
}
trait Literal extends Comparable with Addition{
  def ~(other: Term) = not(other)
  def not(other:Term) =Not(this)
}

trait MapTyped extends Typed

trait ArrayTyped extends Typed


trait Comparable extends Typed{

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

trait Multiply{

  def *(other: Numeric) = mul(other)
  def mul(other:Numeric) = Mul(this, other)
}

trait Appendable extends Typed{
  def append(value:Typed)=Append(this,value)
  def :+(value:Typed)=append(value)
}

trait Countable extends Typed{

  def size = count
  def count=Count(this)
  def count(filter:BooleanPredicate) = Count(this,Some(filter))
  def count(value:Binary)=Count(this,Some((x:Var)=>value))
}
trait Sequence extends Addition with Multiply with Functional with Appendable {


  //def coerceTo(dataType: DataType)=CoerceTo(this,dataType)

  def indexesOf(value:Datum):IndexesOf = IndexesOf(this,Left(value))
  def indexesOf(value:Binary):IndexesOf=indexesOf((x:Var)=>value)

  def isEmpty=IsEmpty(this)

  def sample(amount:Int) = Sample(this,amount)
  def indexesOf(predicate:BooleanPredicate1)=IndexesOf(this,Right(predicate))
  def prepend(value:Typed) = Prepend(this,value)
  def +:(value:Typed) = prepend(value)
  def apply(index:Int) = Nth(this,index)

  def skip(amount: Int) = Skip(this, amount)

  def slice(start: Int = 0, end: Int = -1) = Slice(this, start, end)

  def apply(prange: SliceRange) = Slice(this, prange.start, prange.end)

  def union(sequences: Sequence) = Union(this, sequences)
}

trait Functional extends Typed{
  def map(func: Predicate1) = RMap(this, func)

  def concatMap(func: Predicate1) = ConcatMap(this, func)
}
trait Document extends Functional with Appendable{

  def pluck(attrs: String*) = Pluck(this, attrs)

  def without(attrs: String*) = Without(this, attrs)



  def order(keys: Ordering*) = OrderBy(this, keys)

  def attr(name: String) = GetAttr(this, name)

  def \(name: String) = attr(name)



  def merge(other: Document) = Merge(this, other)

  def +(other: Document) = merge(other)

  def contains(attr: String) = Contains(this, attr)
  def ?(attr:String) =contains(attr)
}

trait Binary extends Typed {

  def &(other: Binary) = and(other)
  def and(other:Binary) = All(this, other)


  def rand(other:Binary) = All(other,this)

  def &>(other: Binary) = rand(other)

  // or
  def |(other: Binary) = or(other)
  def or(other:Binary) = Or(this,other)

  // right or
  def >|(other: Binary) = ror(other)
  def ror(other:Binary) = Or(other, this)
}

trait Strings extends Literal{

  def === (regexp:Regex) = find(regexp.toString())
  def ===(regexp:String) =find(regexp)
  def find(regex:String)=Match(this,regex)
}


trait Numeric extends Literal with Multiply with Binary{

  def -(other: Numeric) = sub(other)
  def sub(other:Numeric) = Sub(this, other)

  //def >-(other: Numeric) = Sub(other, this)



  //def >*(other: Numeric) = Mul(other, this)

  def /(other: Numeric) =div(other)
  def div(other:Numeric) = Div(this, other)



  // def >/(other: Numeric) = Div(other, this)

  def %(other: Numeric) = mod(other)
  def mod(other:Numeric) =Mod(this, other)
}



trait Filterable extends Typed{

  def filter(value:Binary):Func = filter((x:Var)=> value)
  def filter(value:Map[String,Any]):Func = filter((x:Var)=>Expr(value))
  def filter(f:Predicate):Func = Func(f)
}




trait Ref extends Numeric with Binary with Document with Sequence with Comparable with Literal with Strings






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

case class IterableResult(value: Iterable[Any]) extends Result {
  def unwrap = value
}

case class DocumentResult(doc: Document) extends Result {
  def unwrap = doc
}

case class AnyResult(value: Any) extends Result {
  def unwrap = value
}

trait ProduceSequence extends Produce[Iterable[Any]]  with Sequence {



 // def wrap[T <: ProduceSequence#ResultType](value: Any): ProduceSequence#ResultType =  value.asInstanceOf[Iterable[Any]]




  def defaultValue = Iterable.empty[Any]
}

trait ProduceSet extends ProduceSequence



trait ProduceBinary extends Produce[Boolean] with Binary {


 // def wrap[T <: ProduceBinary#ResultType](value: Any): ProduceBinary#ResultType = value.asInstanceOf[Boolean]



  type ResultType = Boolean



  def defaultValue = false
}

//trait ProduceLiteral extends ProduceComparable with Literal

trait ProduceDocument extends Produce[Document]  {

//  type ResultType = Option[Document]

  /*def wrap[T <: ProduceDocument#ResultType](value: Any): ProduceDocument#ResultType = value match{
    case d:Document=>Some(d)
    case _=>None

  } */



  def defaultValue = {
    val d:Document = null
    d
  }

}

trait ProduceNumeric extends Produce[Double]  with  Numeric   {





 /*
  def wrap[T <: ProduceNumeric#ResultType](value: Any): ProduceNumeric#ResultType = value match {
    case d: Double => d
    case i: Int => i.toDouble
    case _ => 0
  }*/

  override def wrap[T <: Double](value: Any): Double = value match {
    case d: Double => d
    case i: Int => i.toDouble
    case _ => 0
  }

  def defaultValue= 0

}



trait ProduceString extends Produce[String] with Strings {





  def defaultValue= ""

 // def wrap[T <: ProduceString#ResultType](value: Any): ProduceString#ResultType = value.toString
  override def wrap[T <: String](value: Any): String = value.toString
}


trait ProduceAny extends Produce[Any] with Ref   {


  override def wrap[T <: Any](value: Any): Any = value

  def defaultValue= AnyRef
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
