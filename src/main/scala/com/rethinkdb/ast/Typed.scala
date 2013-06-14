package com.rethinkdb.ast

import com.rethinkdb.Term
import scala.util.matching.Regex


trait Produce extends Term{
  type ResultType

  import scala.reflect.runtime.universe._


  protected val  extractArgs = true
  private def fields(a: AnyRef) ={

    var f = a.getClass.getDeclaredFields
    f.toSeq.filterNot(_.isSynthetic).take(numConstructorParams(a)).map{field =>
      field.setAccessible(true)
      field
    }
  }
  private def numConstructorParams(a: AnyRef) = a.getClass.getConstructors()(0).getParameterTypes.size
  override lazy val args = if(extractArgs) buildArgs(fields(this).map(_.get(this)): _*) else Seq.empty[Term]

  def withResult[A: TypeTag](result: A): ResultType =
    if (withMapProduce) extract[A, Map[String, Any]](result, defaultValue)(fromMap _)
    else wrap(result)

  val withMapProduce = false

  def wrap(value: Any): ResultType


  def defaultValue: ResultType

  protected def fromMap(m: Map[String, Any]): Any = defaultValue

  protected def extract[A: TypeTag, T: TypeTag](result: A, defaultValue: ResultType)(f: T => Any): ResultType = {
    typeOf[A] match {
      case t if t =:= typeOf[T] => wrap(f(result.asInstanceOf[T]))
      case _ => defaultValue
    }
  }


}

sealed trait Typed{
  implicit def toPredicate1(f: (Var) => Typed) = new Predicate1(f)
  implicit def toBooleanPredicate1(f: (Var) => Binary) = new BooleanPredicate1(f)
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
trait Sequence extends Addition with Multiply with Functional with Appendable{



  def indexesOf(value:Datum) = IndexesOf(this,Left(value))
  def indexesOf(value:Binary)=indexesOf((x:Var)=>value)

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

trait ProduceSequence extends Produce  with Sequence {
  type ResultType = IterableResult

  def wrap(value:Any) = IterableResult(value.asInstanceOf[Iterable[Any]])
  def defaultValue = IterableResult(Iterable.empty[Any])
}

trait ProduceSet extends ProduceSequence



trait ProduceBinary extends Produce with Binary {

  type ResultType = BooleanResult

  def wrap(value: Any) = BooleanResult(value.asInstanceOf[Boolean])

  def defaultValue = BooleanResult(false)
}

//trait ProduceLiteral extends ProduceComparable with Literal

trait ProduceDocument extends Produce  {

  type ResultType = DocumentResult

  def wrap(value: Any)= DocumentResult(value.asInstanceOf[Document])

  def defaultValue = DocumentResult(null)

}

trait ProduceNumeric extends Produce  with  Numeric   {

  type ResultType = NumericResult

  def wrap(value: Any)= {

    val d = value match {
      case d: Double => d
      case i: Int => i.toDouble
      case _ => 0
    }
    NumericResult(d)
  }

   def defaultValue= NumericResult(0)

}



trait ProduceString extends Produce with Strings {

  type ResultType = StringResult

  def wrap(value: Any)= StringResult(value.toString)

  def defaultValue= StringResult("")
}


trait ProduceAny extends Produce with Ref   {

  type ResultType = AnyResult

  def wrap(value: Any)= AnyResult(value)

  def defaultValue= AnyResult(None)
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
