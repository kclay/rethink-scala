package com.rethinkscala.ast

import com.rethinkscala.{JsonDocument, Term, InfoResult, Document}

import ql2.Ql2.Term.TermType
import com.rethinkscala.reflect.Reflector
import org.joda.time.{ReadableInstant, ReadableDateTime, DateTime}
import org.joda.time.format.ISODateTimeFormat
import com.rethinkscala.net.{RethinkClientError, JsonDocumentConversion, WithConversion, RethinkDriverError}
import scala.collection.Iterable

case class MakeArray(array: Seq[Any]) extends Term with ProduceArray {
  override lazy val args = buildArgs(array: _*)

  // def datumTermType:TermType.EnumVal=ql2.Datum
  def termType = TermType.MAKE_ARRAY

}

private[this] object MakeArray {

  def asJson(list: Iterable[Any], depth: Int): MakeArray = new MakeArray(list.toSeq) {
    override lazy val args = buildArgs(Expr.json(array, depth): _*)
  }

}


case class FuncWrap(value: Any) {


  private def scan(node: Any): Boolean = node match {

    case node: ImplicitVar => true
    case t: Term if t.args.collectFirst {
      case arg: Term if scan(arg) => true
    }.getOrElse(false) => true
    case t: Term if t.optargs.collectFirst {
      case p: com.rethinkscala.AssocPair if scan(p.token) => true
    }.getOrElse(false) => true
    case _ => false
  }

  def apply(): Term = {
    val e = Expr(value)
    val rtn = if (scan(value)) new Predicate1((v: Var) => e.asInstanceOf[Typed]).apply() else e
    rtn
  }
}

private[rethinkscala] case class MakeObj2(data: Document) extends Term with MapTyped {

  override protected val extractArgs = false
  override lazy val optargs = buildOptArgs2(Reflector.fields(data).map(f =>
    (f.getName, Expr(f.get(data)))
  ).toMap)

  def termType = TermType.MAKE_OBJ
}


private[this] object MakeObj {


  def asJson(data: Map[String, Any], depth: Int = 20): MakeObj = new MakeObj(data) {
    override lazy val optargs = buildOptArgs2(data.mapValues(v => Expr.json(v, depth)))
  }
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

  def apply(name: String) = this field name
}

case class Info(target: Typed) extends ProduceDocument[InfoResult] {
  def termType = TermType.INFO
}

case class Branch(test: Binary, passed: Typed, failed: Typed) extends ProduceAny {


  def termType = TermType.BRANCH
}

object Core {
  val row = new ImplicitVar()
}

/** Loop over a sequence, evaluating the given write query for each element.
  * @param target
  * @param function
  */
case class ForEach(target: Sequence[_], function: Predicate1) extends ProduceAnyDocument {

  override lazy val args = buildArgs(target, FuncWrap(function))

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

  def apply(f: Double): NumberDatum = NumberDatum(f)

  def apply(d: Document) = MakeObj2(d)

  def apply(date: DateTime) = ISO8601(ISODateTimeFormat.dateTime().print(date))


  def apply(a: Any): Term = a match {
    case w: FuncWrap => w()

    case date: DateTime => apply(date)


    case p: Predicate => p()
    case t: Term => t
    case s: Seq[_] => MakeArray(s)
    case m: Map[_, _] => MakeObj(m.asInstanceOf[Map[String, Option[Any]]])
    case d: Document => MakeObj2(d)
    case a: Any => Datum(a)


  }


  private[this] val DocumentClass = classOf[Document]

  private[this] val ReadableInstantClass = classOf[ReadableInstant]

  private[this] def isJsonClass(d: Class[_], depth: Int): Boolean = {
    val found = Reflector.fields(d).find(f =>
      if (DocumentClass isAssignableFrom f.getType) isJsonClass(f.getType, depth - 1)
      else ReadableInstantClass isAssignableFrom f.getType)

    found.isEmpty

  }

  def isJson(v: Any, depth: Int = 20): Boolean = {
    if (depth < 0) throw RethinkDriverError("Nesting depth limit exceeded")

    v match {
      case t: Term => false
      case m: Map[String, _] => m.find {
        case (a, b) => !isJson(b, depth - 1)
      }.isEmpty
      case d: Document => isJsonClass(d.getClass, depth)
      case l: Iterable[Any] => l.find(r => !isJson(r, depth - 1)).isEmpty
      case Int | Float | Boolean | Double | Long => true

      case a: Any if classOf[java.io.Serializable].isAssignableFrom(a.getClass) && a.getClass.getName.startsWith("java.lang") => true

      // case d: Any if classOf[java.io.Serializable].isAssignableFrom(d.getClass) => throw RethinkDriverError("Use of classes must extend type Document")
      case _ => false

    }
  }

  def json(array: Iterable[Any], depth: Int): Seq[Any] = {
    if (depth < 0) throw RethinkDriverError("Nesting depth limit exceeded")
    array.map(v => json(v, depth - 1)).toSeq
  }

  def json(v: Any, depth: Int = 20): Term = {
    if (depth < 0) throw RethinkDriverError("Nesting depth limit exceeded")
    v match {
      case t: Term => t
      case a: Any if isJson(a, depth) => Json.asLazy(a)

      case d: Map[String, _] => MakeObj.asJson(d, depth)
      case l: Iterable[Any] => MakeArray.asJson(l, depth)
      case a: Any => Expr(a)
    }


  }
}


trait JsonLike


case class LazyJson(value: Any) extends Produce[JsonDocument] with JsonDocumentConversion with JsonLike {


  override lazy val args = buildArgs(Reflector.toJson(value))

  def termType = TermType.JSON
}

case class Json(value: String) extends Produce[JsonDocument] with JsonDocumentConversion with JsonLike {
  def termType = TermType.JSON
}


object Json {
  def asLazy(value: Any) = LazyJson(value)
}