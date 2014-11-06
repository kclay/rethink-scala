package com.rethinkscala

import com.rethinkscala.ast._
import com.rethinkscala.magnets.ReceptacleImplicits
import com.rethinkscala.net._

import scala.collection.Iterable
import scala.concurrent.Future


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

    type InnerProduce = Produce0[String]
  }
  implicit val doubleToNumeric = new ToAst[Double] {
    type TypeMember = Numeric

    type InnerProduce = Produce0[Double]


  }
  implicit val intToNumeric = new ToAst[Int] {
    type TypeMember = Numeric

    type InnerProduce = Produce0[Int]

  }
  implicit val floatToNumeric = new ToAst[Float] {
    type TypeMember = Numeric

    type InnerProduce = Produce0[Float]

  }


  implicit def arrayMapToTyped[T] = new ToAst[Map[String, T]] {
    type TypeMember = Var

    type InnerProduce = Produce0[Map[String, T]]
  }

  implicit def docToTyped[T <: Document] = new ToAst[T] {
    type TypeMember = Var

    type InnerProduce = Produce0[T]

  }

  /*
  implicit def seqToSequence[T>:Any] = new ToAst[Seq[T]] {
    type TypeMember = Sequence[T]

    type InnerProduce = Produce0[T]
  } */

  implicit def seqToAnySequence = new ToAst[Seq[Any]] {
    type TypeMember = Sequence[Any]

    type InnerProduce = Produce0[Any]
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

}

class ToFunctional[T, A >: Var](seq: Sequence[T]) {


  def concatMap[B <: Typed, Inner](f: A => B)(implicit cm: CanMap[T, B, Inner]) = ConcatMap[Inner](seq.underlying, FuncWrap(f))

  def map[B <: Typed, Inner](f: A => B)(implicit cm: CanMap[T, B, Inner]) = RMap[Inner](seq.underlying, FuncWrap(f))

  def reduce[P](f: (A, A) => Produce0[P]) = Reduce[T, P](seq.underlying, f)

}


object CanMap {
  def apply[From, To <: Typed, Out] = new CanMap[From, To, Out]
}

trait CanMapImplicits {


  implicit val mapStringToStrings = CanMap[String, Strings, String]
  implicit val mapStringToNumeric = CanMap[String, Numeric, Int]


  implicit def mapStringToArray[T] = CanMap[String, Sequence[T], T]

  implicit def mapMapToArray[T] = CanMap[Map[String, _], Sequence[T], T]


  implicit val mapIntToNumeric = CanMap[Int, Numeric, Int]
  implicit val mapDoubleToNumeric = CanMap[Double, Numeric, Double]
  implicit val mapFloatToNumeric = CanMap[Float, Numeric, Float]
  implicit val mapLongToNumeric = CanMap[Long, Numeric, Long]


  implicit def mapDocumentToDouble[T <: Document] = CanMap[T, Numeric, Double]

  implicit def mapDocumentToString[T <: Document] = CanMap[T, Strings, String]

  implicit def mapDocumentToAny[T <: Document] = CanMap[T, Ref, Any]


}

class CanMap[-From, -To <: Typed, Out]


private[rethinkscala] trait ImplicitConversions {


  object r extends RethinkApi


  implicit def anyToPimpled(v: Any) = new PimpedAny(v)

  //implicit def toTyped[T<:Typed](v:T):Typed = v

  implicit def collectionToAst[T](coll: Iterable[T]): MakeArray[T] = Expr[T](coll)

  implicit def intToDatNum(i: Int) = NumberDatum(i)

  implicit def longToDatNum(l: Long) = NumberDatum(l)

  implicit def floatToDatNum(f: Float) = NumberDatum(f)

  implicit def doubleToDatNum(d: Double) = NumberDatum(d)

  implicit def string2DatNum(s: String) = StringDatum(s)

  implicit def boolean2Datum(b: Boolean) = BooleanDatum(b)

  /*
 implicit def intToDatNum0(i: Int): Datum = NumberDatum(i)

 implicit def longToDatNum0(l: Long): Datum = NumberDatum(l)

 implicit def floatToDatNum0(f: Float): Datum = NumberDatum(f)

 implicit def doubleToDatNum0(d: Double):Datum = NumberDatum(d)

 implicit def string2DatNum0(s: String): Datum= StringDatum(s)
    */

  implicit def toOptLiteral[T <% Literal](v: T): Option[T] = Some(v)

  implicit def toOptFromDatum[T <% Datum](v: T): Option[T] = Some(v)

  implicit def toPredicate1Opt(f: (Var) => Typed) = Some(new ScalaPredicate1(f))

  implicit def toPredicate2Opt(f: (Var, Var) => Typed) = Some(new ScalaPredicate2(f))

  implicit def toPredicate1(f: Var => Typed) = new ScalaPredicate1(f)

  implicit def toPredicate2(f: (Var, Var) => Typed): ScalaPredicate2 = new ScalaPredicate2(f)

  //implicit def map2Typed(m:Map[String,Any]):Typed = MakeObj(m)
  implicit def map2Typed(m: Map[String, Any]): Typed = Expr(m)

  implicit def untypedPredicateToTyped(f: Var => Map[String, Any]): Predicate1 = (v: Var) => Expr(f(v))


  implicit def toBooleanPredicate1(f: Var => Binary) = new ScalaBooleanPredicate1(f)


  implicit def toBooleanPredicate2(f: (Var, Var) => Binary) = new ScalaBooleanPredicate2(f)


  //implicit def seq2Datum(s:Seq[Datum]) = MakeArray(s)

  implicit def bool2Option(value: Boolean): Option[Boolean] = Some(value)

  implicit def string2Option(value: String): Option[String] = Option(value)

  implicit def double2Option(value: Double): Option[Double] = Some(value)

  implicit def int2Option(value: Int): Option[Int] = Some(value)

  implicit def string2DB(name: String): DB = DB(name)

  case class String2Ast(name: String) {
    def row = r.row(name)

    def asc = Asc(name)

    def desc = Desc(name)
  }

  private def p2t(p: Product): MakeArray[Any] = Expr(p.productIterator.toSeq)

  implicit def tuple2Typed(t: (Typed, Typed)) = p2t(t)

  implicit def tuple3Typed(t: (Typed, Typed, Typed)) = p2t(t)

  implicit def tuple4Typed(t: (Typed, Typed, Typed, Typed)) = p2t(t)

  implicit def string2Ast(name: String) = String2Ast(name)

  implicit def string2Ordering(name: String) = name.asc

  implicit def intWithTildyArrow(start: Int) = new {
    def ~>(end: Int) = SliceRange(start, end)
  }

  implicit def func2Order(f: Var => Typed): Order = new FuncWrap(new ScalaPredicate1(f)) with Order


}

trait FromAst[T] {
  type Raw
}


trait Helpers {

  type Var = com.rethinkscala.ast.Var
  val Expr = com.rethinkscala.ast.Expr
  val Blocking = com.rethinkscala.Implicits.Blocking
  val Async = com.rethinkscala.Implicits.Async
  type BlockingConnection = com.rethinkscala.net.BlockingConnection
  type AsyncConnection = com.rethinkscala.net.AsyncConnection

  type BlockResult[T] = ResultResolver.Blocking[T]
  type AsyncResult[T] = ResultResolver.Async[T]

  def async[T](p: Produce[T])(implicit c: Connection, extractor: ResultExtractor[T]): AsyncResult[T] = async(_.apply(p))

  def async[T](f: AsyncConnection => Future[T])(implicit c: Connection): AsyncResult[T] = f(AsyncConnection(c))

  def block[T](f: BlockingConnection => Unit)(implicit c: Connection): Unit = f(BlockingConnection(c))

  def block[T](f: BlockingConnection => BlockResult[T])(implicit c: Connection): BlockResult[T] = f(BlockingConnection(c))

  def block[T](p: Produce[T])(implicit c: Connection, extractor: ResultExtractor[T]): BlockResult[T] = {

    block { c: BlockingConnection => c.apply(p)}
  }

}

class PimpedAny(val v: Any) extends AnyVal {
  @inline
  def wrap = FuncWrap(v)

  @inline
  def optWrap = Some(FuncWrap(v))
}

object Implicits {


  trait Common extends CanMapImplicits
  with ToAstImplicts
  with ReceptacleImplicits with ImplicitConversions
  with net.Versions
  with Helpers
  with GeometryImplicits {


    implicit val numericToDouble = new FromAst[Numeric] {
      type Raw = Double
    }
    implicit val stringsToString = new FromAst[Strings] {
      type Raw = String
    }

    implicit def arrayToSeq[T](seq: ProduceSequence[T]) = new FromAst[ProduceSequence[T]] {
      type Raw = Seq[T]
    }

    implicit def binaryToBoolean = new FromAst[Binary] {
      type Raw = Boolean
    }


  }

  object Blocking extends net.BlockingImplicits with Common

  object Async extends net.AsyncImplicits with Common

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
