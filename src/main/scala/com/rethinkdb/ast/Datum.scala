package com.rethinkdb.ast


import com.rethinkdb.{DatumAssocPair, DatumMessage, ExprWrap, Composable, AssocPair}
import ql2.Datum.DatumType

sealed trait Datum extends DatumMessage with ExprWrap with Composable {


  override def optArgsBuilder(key: String, value: Any): AssocPair = DatumAssocPair(key, value)


}


object Datum {

  import ql2.Datum.DatumType.{R_NULL, R_BOOL, R_NUM, R_STR, R_ARRAY, R_OBJECT}

  def unapply(datum: ql2.Datum): Any = {
    datum.`type`.get match {
      case R_NULL => None
      case R_BOOL => datum.`rBool`.get
      case R_NUM => datum.`rNum`.map(n => if (n % 1 == 0) n.toInt else n).get
      case R_STR => datum.`rStr`.get
      case R_ARRAY => datum.`rArray`.map(Datum.unapply(_))
      case R_OBJECT => datum.`rObject` map {
        p => (p.`key`.get, Datum.unapply(p.`val`.get))
      } toMap
    }

  }


  def apply(a: Any): Datum = a match {

    case Some(v) => Datum(v)
    case None=> NoneDatum()
    case s: String => StringDatum(s)
    case i: Int => NumberDatum(i)
    case f: Float => NumberDatum(f)
    case l: Long => NumberDatum(l)
    case b: Boolean => BooleanDatum(b)

  }
}


object NoneDatum {
  def apply() = new NoneDatum()
}

class NoneDatum extends Datum {

  def datumType = DatumType.R_NULL

  def build(d: ql2.Datum) = d
}

case class BooleanDatum(value: Boolean) extends Datum {

  def datumType = DatumType.R_BOOL

  def build(d: ql2.Datum) = d.setRBool(value)

}

case class NumberDatum(value: Double) extends Datum {
  def datumType = DatumType.R_NUM

  def build(d: ql2.Datum) = d.setRNum(value)

}

case class StringDatum(value: String) extends Datum {
  def datumType = DatumType.R_STR

  def build(d: ql2.Datum) = d.setRStr(value)

}