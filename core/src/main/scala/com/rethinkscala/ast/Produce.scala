package com.rethinkscala.ast

import com.rethinkscala._
import com.rethinkscala.changefeeds.net.ChangeCursor
import com.rethinkscala.net._
import ql2.Ql2.Term.TermType

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

  def withOptions(opts: Options): this.type = {
    underlyingOptions = opts
    this
  }
}

trait Produce0[T] extends Typed {

  type T0 = T
}

trait ProduceSingle[T] extends Produce[T] with Produce0[T] with CastTo {
  type FieldProduce = ProduceAny

  def apply(name: String): ProduceAny = field(name)

  def field(name: String): ProduceAny = GetField.any(this, name)

}

trait CastTo {
  self: CastTo with Produce0[_] ⇒

  type SeqCast[T] = ToAst.seqToAnySequence.ForType[T]

  def field(name: String): ProduceAny

  def cast[T](name: String)(implicit ast: ToAst[T]): ast.Cast = field(name).asInstanceOf[ast.Cast]

  def int(name: String) = cast[Int](name)

  def double(name: String) = cast[Double](name)

  def long(name: String) = cast[Long](name)

  def float(name: String) = cast[Float](name)

  def string(name: String) = cast[String](name)

  def seq[T](name: String) = castSeq[T](field(name))

  def mapOf[T](name: String) = cast[Map[String, T]](name)

  def anyMap(name: String) = cast[Map[String, Any]](name)

  def anySeq(name: String) = cast[Seq[Any]](name)

  private[rethinkscala] def to[T](implicit ast: ToAst[T]): ast.Cast = this.asInstanceOf[ast.Cast]


  private def castSeq[T](value: Any) = value.asInstanceOf[SeqCast[T]]

  @deprecated("use toInt", "0.4.6")
  def asInt = toInt

  def toInt = to[Int]

  @deprecated("use toInt", "0.4.6")
  def asDouble = toDouble

  def toDouble = to[Double]

  def toLong = to[Long]

  // def toFloat[R](implicit tf:ToCast[CastTo ,Float,R]) = tf.cast

  @deprecated("use toInt", "0.4.6")
  def asString = mkString

  def mkString = to[String]

  @deprecated("use toAnySeq", "0.4.6")
  def asAnySeq = toAnySeq

  def toAnySeq: SeqCast[Any] = toSeq[Any]

  def toSeq[T] = castSeq[T](this)

  @deprecated("use toMap", "0.4.6")
  def asMap = toMap[Any]

  def toMap[T] = to[Map[String, T]]

}

trait ProduceSequenceLike[T, C[_]] extends Sequence[T, C] with Produce0[T] {

  type FieldProduce = ProduceArray[T]

  def field(name: String): ProduceArray[T] = GetField(this, name)

  def merge(other: Map[String, Any]) = Merge(underlying, MakeObj(other))

  // TODO : Fix this
  def merge[R, CR[_]](other: Sequence[R, CR]) = Merge.seq(underlying, other)

  def pluck(attrs: String*) = Pluck(underlying, attrs)

  def without(attrs: String*) = Without(underlying, attrs)

  def pluck(m: Map[String, Any]) = Pluck(underlying, m)

}

trait ProduceSequence[E] extends Typed {

  type Collection[A]
}

trait ProduceSeq[E, C[_]] extends ProduceSequenceLike[E, C] with Produce[C[E]] with ProduceSequence[E] {
  self ⇒

  override type Collection[A] = C[A]
}


trait ProduceDefaultSequence[E] extends ProduceSeq[E, RethinkCursor]

// FIXME support changes ???
trait ProduceAnySequence extends ProduceDefaultSequence[Any]

trait ProduceSet[T] extends ProduceArray[T]

trait ProduceSingleSelection[T] extends SingleSelection[T] with Produce[T] with Produce0[T] with Record {

  override val underlying = this
  type FieldProduce = ProduceAny

  def field(name: String): ProduceAny = GetField.any(underlying, name)

  def merge[R](other: SingleSelection[R]) = Merge.selection(underlying, other)

}

trait ProduceSingleDocumentSelection[T] extends SingleSelection[T] with ProduceDocument[T] {
  override val underlying = this
}

// FIXME support changes ???
trait ProduceStreamSelection[T, C[_]] extends ProduceSeq[T, C] with StreamSelection[T, C]

trait ProduceDefaultStreamSelection[T] extends ProduceStreamSelection[T, RethinkCursor]

trait ProduceChangesSeq[E, C[_]] extends Produce[C[E]] with ProduceSequence[E] /*with Filterable[E, C] */ {
  override type Collection[A] = C[A]
}

trait ProduceChangeStream[T] extends ProduceChangesSeq[CursorChange[T], ChangeCursor]

trait ProduceArray[T] extends ProduceDefaultSequence[T] with ArrayTyped[T]

trait ProduceBinary extends Produce[Boolean] with Binary with Produce0[Boolean]

trait ProduceGroup[T] extends ProduceSeq[T, GroupResult]

trait ProduceDocument[T] extends ProduceSingle[T] with Record with DocumentConversion[T] {

  def merge(other: Map[String, Any]) = Merge.record(underlying, other)

  override def apply(name: String) = field(name)

  def pluck(attrs: String*) = Pluck(underlying, attrs)

  def without(attrs: String*) = Without(underlying, attrs)

  def pluck(m: Map[String, Any]) = Pluck(underlying, m)

}


trait ProduceAnyDocument extends ProduceDocument[Document] with Record

trait ProduceTypedDocument[T <: Document] extends ProduceDocument[T] with Record

trait ProduceNumeric extends ProduceTypedNumeric[Double]

trait ProduceFloat extends ProduceTypedNumeric[Float]

trait ProduceTypedNumeric[@specialized(Int, Double, Long) T] extends ProduceSingle[T] with Numeric with Produce0[T]

trait ProduceString extends ProduceSingle[String] with Strings with Produce0[String] {
  override val underlying = this
}

abstract class ForwardTyped(value: Term) extends Term with Typed {

  override lazy val args = underlyingTerm.args
  override lazy val optargs = underlyingTerm.optargs

  override private[rethinkscala] val underlyingTerm: Term = value

  override def termType: TermType = value.termType
}

//class ToSeq[T](value:Produce0[T]) extends ForwardTyped(value.term) with ProduceSequence[T]

trait ProduceAny extends Produce[Any] with Ref with Produce0[Any] with CastTo {
  any ⇒

  def numeric = new ProduceNumeric {
    override val underlying = any
    override lazy val args = underlyingTerm.args
    override lazy val optargs = underlyingTerm.optargs

    //  override def ast = underlyingTerm.ast

    override private[rethinkscala] val underlyingTerm: Term = any

    def termType: TermType = underlyingTerm.termType

  }

  def string: ProduceString = new ProduceString {

    override lazy val args = underlyingTerm.args
    override lazy val optargs = underlyingTerm.optargs

    override private[rethinkscala] val underlyingTerm: Term = any

    def termType: TermType = underlyingTerm.termType
  }

  def record: Record = this

  def array[T]: ArrayTyped[T] = this.asInstanceOf[ArrayTyped[T]]

  override def merge(other: Any): ProduceAnyDocument = Merge.record(underlying, other)

  type FieldProduce = ProduceAny

  override def \(name: String): ProduceAny = field(name)


  override def apply(name: String): ProduceAny = field(name)

  def +(other: ProduceAny) = add(other)

  def +=(other: ProduceAny) = add(other)

  def add(other: ProduceAny) = Add.any(underlying, other)

  def asArray[T](name: String) = field(name).array[T]

  def field(name: String): ProduceAny = GetField.any(this, name)
}

class MapToDocument[T <: Document](from: Record) extends ProduceTypedDocument[T] {
  override val underlying = from.underlying
  override lazy val args = underlyingTerm.args
  override lazy val optargs = underlyingTerm.optargs

  override private[rethinkscala] val underlyingTerm: Term = from.underlying.asInstanceOf[Term]

  def termType: TermType = underlyingTerm.termType
}

trait ProduceTypedObject[T] extends Produce[Map[String, T]] with Record {
  override type FieldProduce = ProduceAny


  override def field(name: String): ProduceAny = GetField.any(this, name)
}

trait ProduceObject extends Produce[Map[String, Any]] with Record {
  self ⇒
  // def mapTo[T <: Document] = new MapToDocument[T](this)
}

trait ProduceJoin[L, R, C[_]] extends ProduceSeq[JoinResult[L, R], C] with JoinTyped[L, R, C] {

  override val underlying = this
}

trait ProduceTime extends TimeTyped {

  def add(other: Addition) = Add.any(underlying, other)
}