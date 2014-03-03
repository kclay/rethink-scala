package com.rethinkscala.ast

import com.rethinkscala.net._
import com.rethinkscala.{Term, Document}
import com.rethinkscala.net.Connection
import com.rethinkscala.JoinResult


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

trait ProduceSingle[T] extends Produce[T]

trait ProduceSequence[T] extends Sequence[T] with Produce[Seq[T]] {

  type FieldProduce = ProduceTypedArray[T]

  def field(name: String): ProduceTypedArray[T] = GetField[T](this, name)


  // def run(implicit c: Connection, mf: Manifest[T], d: DummyImplicit): Either[RethinkError, Seq[T]] = toQuery[T].toResult


  //def as[R <: T](implicit c: Connection, mf: Manifest[R], d: DummyImplicit): Either[RethinkError, Seq[R]] = toQuery[R].toResult

  //def toOpt(implicit c: Connection, mf: Manifest[T], d: DummyImplicit): Option[Seq[T]] = run fold(x => None, Some(_))


}

trait ProduceAnySequence extends ProduceSequence[Any]

trait ProduceSet extends ProduceArray

trait ProduceBinary extends Produce[Boolean] with Binary

//trait ProduceLiteral extends ProduceLiteral with Literal


trait ProduceDocument[T <: Document] extends ProduceSingle[T] with Record with DocumentConversion[T] {

  type FieldProduce = ProduceAny

  def field(name: String): ProduceAny = GetField(this, name)
}

trait ProduceAnyDocument extends ProduceDocument[Document] with Record

trait ProduceNumeric extends ProduceSingle[Double] with Numeric

trait ProduceString extends ProduceSingle[String] with Strings

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