package com.rethinkscala.ast

import com.rethinkscala.{ DatumAssocPair, DatumMessage, AssocPair }
import ql2.Datum.DatumType

sealed trait Datum extends DatumMessage {

  override def optArgsBuilder(key: String, value: Any): AssocPair = DatumAssocPair(key, value)

}

object Datum {

  import ql2.Datum.DatumType.{ R_NULL, R_BOOL, R_NUM, R_STR, R_ARRAY, R_OBJECT }

  import com.rethinkscala.Implicits.Quick._

  def wrap(datum: ql2.Datum): (Any, String) = {
    val buf = new StringBuilder
    (wrap(datum, buf), buf.toString())

  }

  def wrap(datum: ql2.Datum, buf: StringBuilder): Any = {
    datum.`type` match {
      case Some(R_NULL) => {
        buf ++= "null"
        None
      }
      case Some(R_BOOL) => {
        buf ++= datum.bool.toString
        datum.bool
      }
      case Some(R_NUM) => {
        buf ++= datum.num.toString
        datum.num
      }
      case Some(R_STR) => {
        buf ++= "\"" + datum.str + "\""

        datum.str
      }
      case Some(R_ARRAY) => {
        buf ++= "["
        val len = datum.array.size
        val unwraped = datum.array.zipWithIndex map {
          case (value: ql2.Datum, index: Int) => {
            wrap(value, buf)
            if (index < len) buf + ","
          }
        }
        buf ++= "]"
        unwraped

      }
      case Some(R_OBJECT) => {
        buf ++= "{"
        val len = datum.obj.size

        var index = 0 // FixMe couldn't get .zipWithIndex to work
        val unwrapped = datum.obj.map {
          ap =>
            {
              buf ++= "\"" + ap.`key`.get + "\":"
              val rtn = (ap.`key`.get, wrap(ap.`val`.get, buf))
              index += 1
              if (index < len) buf ++= ","

              rtn
            }

        }
        buf ++= "}"
        unwrapped.toMap

      }
      case _ => None
    }
  }

  def apply(a: Any): Datum = a match {

    case Some(v)    => Datum(v)
    case None       => NoneDatum()
    case s: String  => StringDatum(s)
    case i: Int     => NumberDatum(i)
    case f: Float   => NumberDatum(f)
    case l: Long    => NumberDatum(l)
    case b: Boolean => BooleanDatum(b)
    case d: Double  => NumberDatum(d)
    case _          => NoneDatum()

  }
}

object NoneDatum {
  def apply() = new NoneDatum()
}

class NoneDatum extends Datum {

  def datumType = DatumType.R_NULL

  def build(d: ql2.Datum) = d

  def defaultValue = None
}

case class BooleanDatum(value: Boolean) extends Datum with ProduceBinary {

  override val extractArgs = false

  def datumType = DatumType.R_BOOL

  def build(d: ql2.Datum) = d.setRBool(value)

}

case class NumberDatum(value: Double) extends Datum with ProduceNumeric {

  override val extractArgs = false

  def datumType = DatumType.R_NUM

  def build(d: ql2.Datum) = d.setRNum(value)

}

case class StringDatum(value: String) extends Datum with ProduceString {

  override val extractArgs = false

  def datumType = DatumType.R_STR

  def build(d: ql2.Datum) = d.setRStr(value)

}