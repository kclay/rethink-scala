package com.rethinkscala

import com.rethinkscala.ast._
import com.rethinkscala.ast.Desc
import com.rethinkscala.ast.SliceRange
import com.rethinkscala.ast.Asc
import com.rethinkscala.ast.Var

/** Created with IntelliJ IDEA.
 *  User: keyston
 *  Date: 5/30/13
 *  Time: 6:49 PM
 *  To change this template use File | Settings | File Templates.
 */
object Implicits {



  implicit def boolToDataNum(b: Boolean): BooleanDatum = BooleanDatum(b)

  implicit def intToDatNum(i: Int): NumberDatum = NumberDatum(i)

  implicit def longToDatNum(l: Long): NumberDatum = NumberDatum(l)

  implicit def floatToDatNum(f: Float): NumberDatum = NumberDatum(f)

  implicit def string2DatNum(s: String): StringDatum = StringDatum(s)

  implicit def bool2Option(value: Boolean): Option[Boolean] = Some(value)

  implicit def string2Option(value: String): Option[String] = Some(value)
  implicit def double2Option(value: Double): Option[Double] = Some(value)
  implicit def int2Option(value: Int): Option[Int] = Some(value)

  implicit def string2DB(name: String): DB = DB(name)

  case class String2Ast(name: String) {
    def row = r.row(name)

    def asc = Asc(name)

    def desc = Desc(name)
  }

  implicit def string2Ast(name: String) = String2Ast(name)

  implicit def string2Ordering(name: String) = Asc(name)

  implicit def intWithTildyArrow(i: Int) = new {
    def ~>(j: Int) = SliceRange(i, j)
  }
  implicit def toPredicate1(f: (Var) => Typed) = new Predicate1(f)
  implicit def toPredicate2(f: (Var, Var) => Typed) = new Predicate2(f)

  object Quick{

    case class Q12Datum(datum: ql2.Datum) {
      def bool = datum.`rBool`.get
      def obj = datum.`rObject`
      def str = datum.`rStr`.get
      def num = datum.`rNum`.get
      def array = datum.`rArray`
    }
    implicit def datum2Ql2Datum(d: ql2.Datum) = Q12Datum(d)
    implicit def optdatum2Ql2Datum(d: Option[ql2.Datum]) = Q12Datum(d.get)

    implicit def termAssocPair2Ql2TermAssocPair(p: ql2.Term.AssocPair) = Ql2TermAssocPair(p)

    implicit def term2Q12Term(t: ql2.Term) = Ql2Term(t)

    implicit def optTerm2Ql2Term(t: Option[ql2.Term]) = Ql2Term(t.get)

    case class Ql2Term(term: ql2.Term) {
      def datum = term.`datum`
      def bool = datum.bool
      def obj = datum.obj
      def str = datum.str
      def num = datum.num
      def array: Either[Seq[ql2.Term], Seq[ql2.Datum]] = term.`type` match {
        case Some(ql2.Term.TermType.MAKE_ARRAY) => Left(term.`args`)
        case _                                  => Right(datum.array)
      }
    }

    case class Ql2TermAssocPair(p: ql2.Term.AssocPair) {
      val key = p.`key`.get
      val value = p.`val`
      def bool = value.bool
      def obj = value.obj
      def str = value.str
      def num = value.num
      def array = value.array
    }
  }

}
