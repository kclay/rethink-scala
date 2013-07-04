package com.rethinkscala.ast

import com.rethinkscala.{ Term }

import ql2.Term.TermType
import com.rethinkscala.reflect.Reflector
import com.rethinkscala.net.{InfoResult, Document}

case class MakeArray(array: Seq[Any]) extends Term with ArrayTyped {
  override lazy val args = buildArgs(array: _*)

  // def datumTermType:TermType.EnumVal=ql2.Datum
  def termType = TermType.MAKE_ARRAY

}

case class MakeObj(data: Map[String, Any]) extends Term with MapTyped {

  override protected val extractArgs = false
  override lazy val optargs = buildOptArgs2(data)

  def termType = TermType.MAKE_OBJ
}

case class Var(id: Int) extends Term with ProduceAny {
  override lazy val args = buildArgs(id)

  def termType = TermType.VAR
}

case class JavaScript(code: String, timeout: Option[Int] = None) extends Term {
  override lazy val args = buildArgs(code)

  override lazy val optargs = buildOptArgs(Map("timeout" -> timeout))

  def termType = TermType.JAVASCRIPT
}

case class UserError(error: String) extends Term {

  def termType = TermType.ERROR
}

class ImplicitVar extends Term with ProduceAny {

  def termType = TermType.IMPLICIT_VAR
}

case class Info(target: Typed) extends ProduceDocument[InfoResult] {
  def termType = TermType.INFO
}

case class Branch(test: BooleanPredicate, passed: Typed, failed: Typed) extends ProduceAny {
  def termType = TermType.BRANCH
}

object Core {
  val row = new ImplicitVar()
}

/** Loop over a sequence, evaluating the given write query for each element.
 *  @param target
 *  @param function
 */
case class ForEach(target: Sequence, function: Predicate1) extends ProduceAnyDocument {

  override lazy val args = buildArgs(target, function())

  def termType = TermType.FOREACH
}

case class CoerceTo(target: Typed, dataType: DataType) extends ProduceAny {

  override lazy val args = buildArgs(target, dataType.name)

  def termType = TermType.COERCE_TO
}

case class FuncCall(function: Predicate, values: Seq[Typed]) extends ProduceAny {

  override lazy val args = buildArgs(values.+:(function()): _*)

  def termType = TermType.FUNCALL
}

object Expr {

  def apply(term: Term): Term = term

  def apply(value: Seq[Any]): MakeArray = MakeArray(value)

  def apply(value: Map[String, Any]): MakeObj = MakeObj(value)

  def apply(value: String): StringDatum = StringDatum(value)

  def apply(b: Boolean): BooleanDatum = BooleanDatum(b)

  def apply(i: Int): NumberDatum = NumberDatum(i)

  def apply(l: Long): NumberDatum = NumberDatum(l)

  def apply(f: Float): NumberDatum = NumberDatum(f)

  def apply(d: Document) = MakeObj(Reflector.toMap(d))

  def apply(a: Any): Term = {
    val b = a
    a match {
      case t: Term      => t
      case s: Seq[_]    => MakeArray(s)
      case m: Map[_, _] => MakeObj(m.asInstanceOf[Map[String, Option[Any]]])
      case d: Document  => apply(d)
      case a: Any       => Datum(a)

    }
  }

}
