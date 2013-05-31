package com



import scala.Some



/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 3/19/13
 * Time: 7:32 PM 
 */
package object rethinkdb {
  import com.rethinkdb.ast.{DB, StringDatum, NumberDatum, BooleanDatum}


  implicit def boolToDataNum(b: Boolean): Term= BooleanDatum(b)

  implicit def intToDatNum(i: Int): Term = NumberDatum(i)

  implicit def longToDatNum(l: Long): Term = NumberDatum(l)

  implicit def floatToDatNum(f: Float): Term= NumberDatum(f)

  implicit def string2DatNum(s: String): StringDatum = StringDatum(s)

  implicit def bool2Option(value: Boolean): Option[Boolean] = Some(value)

  implicit def string2Option(value: String): Option[String] = Some(value)
  implicit def double2Option(value: Double): Option[Double] = Some(value)
  implicit def int2Option(value: Int): Option[Int] = Some(value)

  implicit def string2DB(name: String): DB = DB(name)

  case class Q12Datum(datum:ql2.Datum){
    def bool = datum.`rBool`
    def obj = datum.`rObject`
    def str = datum.`rStr`
    def num = datum.`rNum`
    def array=datum.`rArray`
  }
  implicit def datum2Ql2Datum(d:ql2.Datum)=Q12Datum(d)
  implicit def optdatum2Ql2Datum(d:Option[ql2.Datum]) =Q12Datum(d.get)


  implicit def termAssocPair2Ql2TermAssocPair(p:ql2.Term.AssocPair) = Ql2TermAssocPair(p)

  implicit def term2Q12Term(t:ql2.Term) = Ql2Term(t)

  implicit def optTerm2Ql2Term(t:Option[ql2.Term]) = Ql2Term(t.get)

  case class Ql2Term(term:ql2.Term){
    def datum = term.`datum`
    def bool = datum.bool
    def obj = datum.obj
    def str = datum.str
    def num = datum.num
    def array:Either[Seq[ql2.Term],Seq[ql2.Datum]]=term.`type` match{
      case Some(ql2.Term.TermType.MAKE_ARRAY)=> Left(term.`args`)
      case _=> Right(datum.array)
    }
  }


  case class Ql2TermAssocPair(p:ql2.Term.AssocPair){
    val key= p.`key`.get
    val value = p.`val`
    def bool = value.bool
    def obj = value.obj
    def str = value.str
    def num = value.num
    def array=value.array
  }





}
