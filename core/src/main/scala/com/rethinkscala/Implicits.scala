package com.rethinkscala

import com.rethinkscala.ast._
import com.rethinkscala.backend.netty
import com.rethinkscala.changefeeds.ast.Changes
import com.rethinkscala.magnets.ReceptacleImplicits
import com.rethinkscala.net._

import scala.collection.Iterable
import scala.concurrent.Future
import com.rethinkscala.backend.{Connection => BackendConnection}


/** Created with IntelliJ IDEA.
  * User: keyston
  * Date: 5/30/13
  * Time: 6:49 PM
  * To change this template use File | Settings | File Templates.
  *
  *
  */


trait ToAstImplicts {

  implicit val stringToStrings = new ToAst[String] {
    type TypeMember = Strings
    type ProduceDefault = ProduceString

    type InnerProduce = Produce0[String]

    override def default(target: Typed, value: String) = new Default(target, value) with ProduceString
  }
  implicit val doubleToNumeric = new ToAst[Double] {
    type TypeMember = Numeric

    type InnerProduce = Produce0[Double]
    type ProduceDefault = ProduceTypedNumeric[Double]

    override def default(target: Typed, value: Double) = new Default(target, value) with ProduceTypedNumeric[Double]

  }
  implicit val longToNumeric = new ToAst[Long] {
    type TypeMember = Numeric

    type InnerProduce = Produce0[Long]
    type ProduceDefault = ProduceTypedNumeric[Long]

    override def default(target: Typed, value: Long) = new Default(target, value) with ProduceTypedNumeric[Long]

  }
  implicit val intToNumeric = new ToAst[Int] {
    type TypeMember = Numeric

    type InnerProduce = Produce0[Int]
    type ProduceDefault = ProduceTypedNumeric[Int]

    override def default(target: Typed, value: Int) = new Default(target, value) with ProduceTypedNumeric[Int]

  }
  implicit val floatToNumeric = new ToAst[Float] {
    type TypeMember = Numeric

    type ProduceDefault = ProduceTypedNumeric[Float]
    type InnerProduce = Produce0[Float]

    override def default(target: Typed, value: Float) = new Default(target, value) with ProduceTypedNumeric[Float]
  }


  implicit def arrayMapToTyped[T] = new ToAst[Map[String, T]] {
    type TypeMember = Var

    type ProduceDefault = ProduceTypedObject[T]
    type InnerProduce = Produce0[Map[String, T]]

    override def default(target: Typed, value: Map[String, T]) = new Default(target, value) with ProduceTypedObject[T]
  }

  implicit def docToTyped[T <: Document] = new ToAst[T] {
    type TypeMember = Var

    type InnerProduce = Produce0[T]
    type ProduceDefault = ProduceDocument[T]

    override def default(target: Typed, value: T) = new Default(target, value) with ProduceDocument[T]
  }


  implicit def seqToSequence[T] = new ToAst[Seq[T]] {
    type TypeMember = Sequence[Any, RethinkCursor]
    type InnerProduce = Produce0[T]
    type ProduceDefault = ProduceDefaultSequence[T]

    //type ForType[T] = Sequence[T, DefaultCursor] with Produce[Seq[T]] with Produce0[T]
    override def default(target: Typed, value: Seq[T]) = new Default(target, value) with ProduceDefaultSequence[T]
  }

  implicit lazy val seqToAnySequence = new ToAst[Seq[Any]] {
    type TypeMember = Sequence[Any, RethinkCursor]

    type InnerProduce = Produce0[Any]
    type ForType[T] = Sequence[T, RethinkCursor] with Produce[Seq[T]] with Produce0[T]
    type ProduceDefault = ProduceDefaultSequence[Any]

    override def default(target: Typed, value: Seq[Any]) = new Default(target, value) with ProduceDefaultSequence[Any]
  }
}

object ToAst extends ToAstImplicts

trait ToAst[A] {
  self: ToAst[A] =>
  type Type = A
  type TypeMember >: Var
  type Producer = Produce[A]
  type InnerProduce <: Produce0[_]
  type Cast = TypeMember with InnerProduce with Producer
  type ProduceDefault <: Typed

  def default(target: Typed, value: A): ProduceDefault

}

trait ToFloat[From, Result] {

  @deprecated("use toFloat", "0.4.6")
  def asFloat: Result = toFloat

  def toFloat: Result
}


trait ToFloatLowerImplicits {
  self: ToAstImplicts =>

  implicit def castToFloat(target: CastTo): ToFloat[CastTo, floatToNumeric.Cast] = new ToFloat[CastTo, floatToNumeric.Cast] {


    override def toFloat = target.asInstanceOf[floatToNumeric.Cast]
  }
}

trait ToFloatImplicits extends ToFloatLowerImplicits {
  self: ToAstImplicts =>
  type RandomFloat[T, R] = Random[T, R] with ProduceFloat
  type RandomType[T] = Random[T, T] with ProduceTypedNumeric[T]

  implicit def randomToFloat[T](target: RandomType[T]): ToFloat[RandomType[T], RandomFloat[T, Float]] = new ToFloat[RandomType[T], RandomFloat[T, Float]] {
    override def toFloat = new Random[T, Float](target.values, Some(true)) with ProduceFloat
  }

}

class ToFunctional[T, A >: Var, C[_]](val seq: Sequence[T, C]) extends AnyVal {

  def concatMap[B <: Typed, Result](f: A => B)(implicit cm: CanMap[T, B, Result]) = ConcatMap[T, C, Result](seq.underlying, FuncWrap(f))

  def map[B <: Typed, Result](f: A => B)(implicit cm: CanMap[T, B, Result]) = RMap[T, C, Result](seq.underlying, FuncWrap(f))

  def reduce[P](f: (A, A) => Produce0[P]) = Reduce[T, P](seq.underlying, f)

}

/*class ToAnyFunctional(val any: ProduceAny) extends AnyVal {
 def map[B<:Typed,Result](f:Var=> B)(implicit cm:CanMap[ProduceAny,B,Result]) = RMap[ProduceAny,]
} */

object CanMap {
  def apply[From, To <: Typed, Out] = new CanMap[From, To, Out]
}

trait CanMapImplicits {


  implicit val mapStringToStrings = CanMap[String, Strings, String]

  implicit val mapStringToNumeric = CanMap[String, Numeric, Int]
  implicit val mapStringToDouble = CanMap[String, Numeric, Double]
  implicit val mapStringToFloat = CanMap[String, Numeric, Float]
  implicit val mapStringToLong = CanMap[String, Numeric, Long]


  implicit def mapStringToArray[T, C[_]] = CanMap[String, Sequence[T, C], T]

  implicit def mapMapToArray[T, C[_]] = CanMap[Map[String, _], Sequence[T, C], T]


  implicit val mapIntToNumeric = CanMap[Int, Numeric, Int]
  implicit val mapDoubleToNumeric = CanMap[Double, Numeric, Double]
  implicit val mapFloatToNumeric = CanMap[Float, Numeric, Float]
  implicit val mapLongToNumeric = CanMap[Long, Numeric, Long]


  implicit def mapDocumentToDouble[T <: AnyRef] = CanMap[T, Numeric, Double]


  implicit def mapDocumentToString[T <: AnyRef] = CanMap[T, Strings, String]

  implicit def mapDocumentToDocument[T <: AnyRef, D <: Document]: CanMap[T, ProduceDocument[D], T] = CanMap[T, ProduceDocument[D], T]


  implicit def mapDocumentToAny[T <: AnyRef] = CanMap[T, Ref, Any]


}

class CanMap[-From, -To <: Typed, Out]

trait DefaultValue[T] {
  type Produce
}


trait LiteralConversions {
  implicit def intToDatNum(i: Int): NumberDatum = NumberDatum(i)

  implicit def longToDatNum(l: Long): NumberDatum = NumberDatum(l)

  implicit def floatToDatNum(f: Float): NumberDatum = NumberDatum(f)

  implicit def doubleToDatNum(d: Double): NumberDatum = NumberDatum(d)

  implicit def string2DatNum(s: String): StringDatum = StringDatum(s)

  implicit def boolean2Datum(b: Boolean): BooleanDatum = BooleanDatum(b)


  implicit def toOptLiteral[T <% Literal](v: T): Option[T] = Some(v)

  implicit def toOptFromDatum[T <% Datum](v: T): Option[T] = Some(v)
}

trait FunctionConversions {

  implicit def toPredicate1Opt(f: (Var) => Typed): Option[Predicate1] = Some(new ScalaPredicate1(f))

  implicit def toPredicate2Opt(f: (Var, Var) => Typed): Option[Predicate2] = Some(new ScalaPredicate2(f))

  implicit def toPredicate1(f: Var => Typed): Predicate1 = new ScalaPredicate1(f)

  implicit def toPredicate2(f: (Var, Var) => Typed): ScalaPredicate2 = new ScalaPredicate2(f)

  implicit def untypedPredicateToTyped(f: Var => Map[String, Any]): Predicate1 = (v: Var) => Expr(f(v))


  implicit def toBooleanPredicate1(f: Var => Binary): BooleanPredicate1 = new ScalaBooleanPredicate1(f)


  implicit def toBooleanPredicate2(f: (Var, Var) => Binary): BooleanPredicate2 = new ScalaBooleanPredicate2(f)

}

final class SliceSupport(val start: Int) extends AnyVal {
  def ~>(end: Int): SliceRange = SliceRange(start, end)
}

private[rethinkscala] trait ImplicitConversions extends LiteralConversions with FunctionConversions {


  implicit def anyToPimpled(v: Any): PimpedAny = new PimpedAny(v)


  implicit def collectionToAst[T](coll: Iterable[T]): MakeArray[T] = Expr[T](coll)


  implicit def toOptFromWrappedValue[T <: WrappedValue[_]](v: T): Option[T] = Some(v)


  implicit def map2Typed(m: Map[String, Any]): Typed = Expr(m)


  implicit def string2DB(name: String): DB = DB(name)

  implicit def bool2Option(value: Boolean): Option[Boolean] = Some(value)

  implicit def string2Option(value: String): Option[String] = Option(value)

  implicit def double2Option(value: Double): Option[Double] = Some(value)

  implicit def int2Option(value: Int): Option[Int] = Some(value)

  case class String2Ast(name: String) {
    def row: ProduceAny = RethinkApi.row(name)

    def asc: Asc = Asc(name.wrap)

    def desc: Desc = Desc(name.wrap)
  }

  private def p2t(p: Product): MakeArray[Any] = Expr(p.productIterator.toSeq)

  implicit def tuple2Typed(t: (Typed, Typed)): MakeArray[Any] = p2t(t)

  implicit def tuple3Typed(t: (Typed, Typed, Typed)): MakeArray[Any] = p2t(t)

  implicit def tuple4Typed(t: (Typed, Typed, Typed, Typed)): MakeArray[Any] = p2t(t)

  implicit def string2Ast(name: String): String2Ast = String2Ast(name)

  implicit def string2Ordering(name: String): Asc = name.asc


  implicit def intWithTildyArrow(start: Int): SliceSupport = new SliceSupport(start)


  implicit def func2Order(f: Var => Typed): Order = new FuncWrap(new ScalaPredicate1(f)) with Order


}

trait FromAst[T] {
  type Raw
}


trait Helpers {


  object Backends {



    val Blocking = netty.blocking.BlockingBackend
    val Async = netty.async.AsyncBackend
  }

  type Var = com.rethinkscala.ast.Var
  val Expr = com.rethinkscala.ast.Expr
  val Blocking = Backends.Blocking.profile
  val Async = Backends.Async.profile
  type BlockingConnection = Backends.Blocking.ConnectionDef
  type AsyncConnection = Backends.Async.ConnectionDef
  val AsyncConnection = Backends.Async.Connection
  val BlockingConnection = Backends.Blocking.Connection
  type BlockResult[T] = Backends.Blocking.Result[T]
  type AsyncResult[T] = Backends.Async.Result[T]

  def async[T](p: Produce[T])(implicit c: BackendConnection, extractor: ResultExtractor[T]): AsyncResult[T] = async(_.apply(p))

  def async[T](f: AsyncConnection => Future[T])(implicit c: BackendConnection): AsyncResult[T] = f(AsyncConnection(c))

  def block[T](f: BlockingConnection => Unit)(implicit c: BackendConnection): Unit = f(BlockingConnection(c))

  def block[T](f: BlockingConnection => BlockResult[T])(implicit c: BackendConnection): BlockResult[T] = f(BlockingConnection(c))

  def block[T: Manifest](p: Produce[T])(implicit c: BackendConnection): BlockResult[T] = {

    block { bc: BlockingConnection =>
      implicit val extractor = bc.resultExtractorFactory.create[T]
      bc.apply(p)
    }
  }

}

class PimpedAny(val v: Any) extends AnyVal {
  @inline
  def wrap: FuncWrap = FuncWrap(v)

  @inline
  def optWrap: Option[FuncWrap] = Some(FuncWrap(v))
}


final class ChangeFeedSupport[T](val target: Typed) extends AnyVal {
  def changes(squash: Option[Boolean] = None, includeStates: Option[Boolean] = None): Changes[T] = new Changes[T](target, squash, includeStates)

  def changes(): Changes[T] = new Changes[T](target)
}


object Implicits {


  trait Common extends CanMapImplicits
    with ToAstImplicts
    with ReceptacleImplicits with ImplicitConversions
    with net.Versions
    with Helpers
    with ToFloatImplicits
    with GeometryImplicits {

    object r extends RethinkApi

    implicit def toChangeFeed[T](typed: Produce0[T]): ChangeFeedSupport[T] = new ChangeFeedSupport[T](typed)

    implicit val numericToDouble = new FromAst[Numeric] {
      type Raw = Double
    }
    implicit val stringsToString = new FromAst[Strings] {
      type Raw = String
    }

    implicit def arrayToSeq[T](seq: ProduceSequence[T]): FromAst[ProduceSequence[T]] = new FromAst[ProduceSequence[T]] {
      type Raw = Seq[T]
    }

    implicit def binaryToBoolean: FromAst[Binary] = new FromAst[Binary] {
      type Raw = Boolean
    }


  }


  object Quick {

    import ql2.{Ql2 => ql2}

    import scala.collection.JavaConverters._

    case class Q12Datum(datum: ql2.Datum) {
      def bool = datum.getRBool

      def obj = datum.getRObjectList.asScala

      def str = datum.getRStr

      def num = datum.getRNum

      def array = datum.getRArrayList.asScala
    }

    implicit def datum2Ql2Datum(d: ql2.Datum) = Q12Datum(d)

    implicit def optdatum2Ql2Datum(d: Option[ql2.Datum]) = Q12Datum(d.get)

    implicit def termAssocPair2Ql2TermAssocPair(p: ql2.Term.AssocPair) = Ql2TermAssocPair(p)

    implicit def term2Q12Term(t: ql2.Term) = Ql2Term(t)

    implicit def optTerm2Ql2Term(t: Option[ql2.Term]) = Ql2Term(t.get)

    case class Ql2Term(term: ql2.Term) {
      def datum = term.getDatum

      def bool = datum.bool

      def obj = datum.obj

      def str = datum.str

      def num = datum.num

      def array: Either[Seq[ql2.Term], Seq[ql2.Datum]] = term.getType match {
        case ql2.Term.TermType.MAKE_ARRAY => Left(term.getArgsList.asScala)
        case _ => Right(datum.array)
      }
    }

    case class Ql2TermAssocPair(p: ql2.Term.AssocPair) {
      val key = p.getKey
      val value = p.getVal

      def bool = value.bool

      def obj = value.obj

      def str = value.str

      def num = value.num

      def array = value.array
    }


  }

}
