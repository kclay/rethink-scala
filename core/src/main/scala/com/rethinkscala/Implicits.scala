package com.rethinkscala

import com.rethinkscala.ast._

import com.rethinkscala.utils.{Applicator1, Applicator2}
import com.rethinkscala.ast.StringDatum
import com.rethinkscala.ast.SliceRange
import com.rethinkscala.ast.Var
import com.rethinkscala.ast.BooleanPredicate2
import com.rethinkscala.ast.DB
import com.rethinkscala.ast.Asc
import scala.Some
import com.rethinkscala.ast.FuncWrap
import com.rethinkscala.ast.Desc
import com.rethinkscala.ast.BooleanDatum
import com.rethinkscala.ast.BooleanPredicate1

import com.rethinkscala.ast.NumberDatum
import scala.collection.Iterable


/** Created with IntelliJ IDEA.
  * User: keyston
  * Date: 5/30/13
  * Time: 6:49 PM
  * To change this template use File | Settings | File Templates.
  *
  *
  */


trait ToAst[A] {
  self: ToAst[A] =>
  type TypeMember >: Var




  def apply2[R](f: ((Var, Var) => Typed) => R) = new Applicator2[TypeMember, R] {
    protected def _apply = f(this.view)
  }

  def wrap2[R](f: FuncWrap => R) = apply2(o => f(FuncWrap(o)))

  def wrap[R](f: FuncWrap => R) = apply(o => f(FuncWrap(o)))

  def wrap3[R, T](f: FuncWrap => R) = apply3[R, T](o => f(FuncWrap(o)))

  def apply3[R, T](f: (Var => Typed) => R) = new Applicator1[T, R] {
    protected def _apply = f(this.view)
  }

  def apply[R](f: (Var => Typed) => R) = new Applicator1[this.type#TypeMember, R] {
    protected def _apply = f(this.view)
  }


}

private[rethinkscala] trait ImplicitConversions {


  implicit def boolToDataNum(b: Boolean): Binary = BooleanDatum(b)



  //implicit def toTyped[T<:Typed](v:T):Typed = v

  implicit def collectionToAst[T](coll:Iterable[T]):MakeArray[T] = Expr[T](coll)
  implicit def intToDatNum(i: Int): Numeric = NumberDatum(i)

  implicit def longToDatNum(l: Long): Numeric = NumberDatum(l)

  implicit def floatToDatNum(f: Float): Numeric = NumberDatum(f)

  implicit def doubleToDatNum(d: Double): Numeric = NumberDatum(d)

  implicit def string2DatNum(s: String): Strings = StringDatum(s)


  implicit def toOptLiteral[T <% Literal](v: T): Option[T] = Some(v)

  implicit def toOptFromDatum[T <% Datum](v: T): Option[T] = Some(v)

  implicit def toPredicate1Opt(f: (Var) => Typed) = Some(new Predicate1(f))

  implicit def toPredicate2Opt(f: (Var, Var) => Typed) = Some(new Predicate2(f))

  implicit def toPredicate1(f: Var => Typed) = new Predicate1(f)

  implicit def toPredicate2(f: (Var, Var) => Typed): Predicate2 = new Predicate2(f)

  //implicit def map2Typed(m:Map[String,Any]):Typed = MakeObj(m)
  implicit def map2Typed(m: Map[String, Any]): Typed = Expr(m)

  implicit def untypedPredicateToTyped(f: Var => Map[String, Any]): Predicate1 = (v: Var) => Expr(f(v))


  implicit def toBooleanPredicate1(f: (Var) => Binary) = new BooleanPredicate1(f)


  implicit def toBooleanPredicate2(f: (Var, Var) => Binary) = new BooleanPredicate2(f)


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

  implicit def func2Order(f: Var => Typed): Order = new FuncWrap(new Predicate1(f)) with Order


}

object Implicits extends ImplicitConversions {


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
