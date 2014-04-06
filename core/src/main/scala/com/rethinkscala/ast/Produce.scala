package com.rethinkscala.ast

import com.rethinkscala.net._
import com.rethinkscala._
import com.rethinkscala.net.Connection
import com.rethinkscala.JoinResult
import com.rethinkscala.net.BlockingResultQuery


/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 2/24/14
 * Time: 8:11 AM 
 */


trait Produce[ResultType] extends Query {


  type resultType = ResultType

  type Options = Map[String, Any]


  private[rethinkscala] var underlyingOptions: Options = Map()

  private[rethinkscala] val underlyingTerm: Term = this

  def toQuery[R](implicit c: Connection, tt: Manifest[R]): BlockingResultQuery[R] = new BlockingResultQuery[R](underlyingTerm, c.asInstanceOf[BlockingConnection], tt, underlyingOptions)

  //http://stackoverflow.com/a/3461734


  //def asJson(implicit c: Connection) = toQuery[String].toResult

  //  def run(implicit c: Connection, mf: Manifest[ResultType]): Either[RethinkError, ResultType] = toQuery.toResult

  def withOptions(opts: Options): this.type = {
    underlyingOptions = opts
    this
  }

  //def profile(implicit c: Connection) = withOptions(underlyingOptions ++ Map("profile" -> true)).toQuery[QueryProfile[ResultType]].toResult


  // def as[R <: ResultType](implicit c: Connection, tt: Manifest[R]): Either[RethinkError, R] = toQuery.toResult


  //def toOpt(implicit c: Connection, mf: Manifest[ResultType]) = run fold(x => None, Some(_))


  // def asOpt[R <: ResultType](implicit c: Connection, tt: Manifest[R], d: DummyImplicit) = as[R] fold(x => None, Some(_))


}

sealed trait Produce0[T] extends Typed
trait ProduceSingle[T] extends Produce[T] with Produce0[T]




trait ProduceSequenceLike[T]  extends Sequence[T] with Produce0[T]    with CanManipulate[SPluck,Merge,Without]{
  type FieldProduce = ProduceArray[T]


  def field(name: String): ProduceArray[T] = GetField[T](this, name)




  // def run(implicit c: Connection, mf: Manifest[T], d: DummyImplicit): Either[RethinkError, Seq[T]] = toQuery[T].toResult


  //def as[R <: T](implicit c: Connection, mf: Manifest[R], d: DummyImplicit): Either[RethinkError, Seq[R]] = toQuery[R].toResult



  override def merge(other: Map[String, Any]) = Merge(underlying,other)


  // TODO : Fix this
  override def merge(other: CM) = Merge(underlying.asInstanceOf[CM],other)

  def pluck(attrs: String*) = Pluck(underlying, attrs)

  def without(attrs: String*) = Without(underlying, attrs)

  def pluck(m: Map[String, Any]) = Pluck(underlying, m)

  def merge(other: MakeObj) = Merge(underlying, other)

}

trait ProduceSequence[T]  extends ProduceSequenceLike[T]  with Produce[Seq[T]]

trait ProduceAnySequence extends ProduceSequence[Any]

trait ProduceSet[T] extends ProduceArray[T]

trait ProduceBinary extends Produce[Boolean] with Binary with Produce0[Boolean]

//trait ProduceLiteral extends ProduceLiteral with Literal


trait ProduceGroup[T] extends Produce[GroupResult[T]] with ProduceSequenceLike[GroupResult[T]]

trait ProduceDocument[T <: Document] extends ProduceSingle[T] with Record with DocumentConversion[T] with CanManipulate[OPluck,Merge,Without] {

  type FieldProduce = ProduceAny

  def field(name: String): ProduceAny = GetField(this, name)



  override def merge(other: Map[String, Any]) = Merge(underlying,other)


  // TODO : Fix this
  override def merge(other: CM) = Merge(underlying.asInstanceOf[CM],other)

  def pluck(attrs: String*) = Pluck(underlying, attrs)

  def without(attrs: String*) = Without(underlying, attrs)

  def pluck(m: Map[String, Any]) = Pluck(underlying, m)


}

trait ProduceAnyDocument extends ProduceDocument[Document] with Record

trait ProduceNumeric extends ProduceSingle[Double] with Numeric with Produce0[Double]

trait ProduceString extends ProduceSingle[String] with Strings   with Produce0[String]

trait ForwardTyped {
  self: Produce[_] with Typed =>
  override lazy val args = underlyingTerm.args
  override lazy val optargs = underlyingTerm.optargs

  override def ast = underlyingTerm.ast

  override protected val underlyingTerm: Term = underlying.asInstanceOf[Term]

  def termType = underlying.term
}

trait ProduceAny extends Produce[Any] with Ref with Produce0[Any] {


  // def numeric: ProduceNumeric =
  private[rethinkscala] val any = this


  def numeric = new ProduceNumeric {
    override val underlying = any
    override lazy val args = underlyingTerm.args
    override lazy val optargs = underlyingTerm.optargs

    override def ast = underlyingTerm.ast

    override private[rethinkscala] val underlyingTerm: Term = any

    def termType = underlyingTerm.termType


  }

  def string: ProduceString = new ProduceString {
    override val underlying = any
    override lazy val args = underlyingTerm.args
    override lazy val optargs = underlyingTerm.optargs

    override def ast = underlyingTerm.ast

    override private[rethinkscala] val underlyingTerm: Term = any


    def termType = underlyingTerm.termType
  }


  def record: Record = this

  def array[T]: ArrayTyped[T] = this.asInstanceOf[ArrayTyped[T]]


  type FieldProduce = ProduceAny

  override def \(name: String): ProduceAny = field(name)

 def as[T](name: String)(implicit ast: ToAst[T]) = field(name).asInstanceOf[ast.TypeMember with  Produce[T]]
 def asArray[T](name:String)=field(name).array[T]
  def field(name: String) = GetField(this.asInstanceOf[Typed], name)
}


trait ProduceSingleSelection[T <: Document] extends SingleSelection[T] with ProduceDocument[T]


trait ProduceStreamSelection[T] extends ProduceSequence[T] with StreamSelection[T]

trait ProduceArray[T] extends ProduceSequence[T] with ArrayTyped[T]


trait ProduceJoin[L, R] extends ProduceSequence[JoinResult[L, R]] with JoinTyped[L, R] {
  override val underlying = this
}

trait ProduceTime extends TimeTyped {
  def add(other: Addition) = AnyAdd(underlying, other)
}