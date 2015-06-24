package com.rethinkscala.ast

import com.rethinkscala._
import com.rethinkscala.net.{JsonDocumentConversion, RethinkDriverError}
import com.rethinkscala.reflect.Reflector
import org.joda.time.ReadableInstant
import org.joda.time.format.ISODateTimeFormat
import ql2.Ql2.Term.TermType

import scala.collection.Iterable

case class MakeArray[T](array: Iterable[T]) extends ProduceArray[T] {
  override lazy val args = buildArgs(array.toSeq: _*)

  def termType: TermType = TermType.MAKE_ARRAY

}

private[this] object MakeArray {

  def asJson(list: Iterable[Any], depth: Int) = new MakeArray(list) {
    override lazy val args = buildArgs(Expr.json(array, depth))
  }

  def apply[T](array: Iterable[T], depth: Int) = new MakeArray[T](array) {
    override lazy val args = buildArgs2(depth - 1, array.toSeq: _*)
  }

}

case class FuncWrap(value: Any) {

  private def scan(node: Any): Boolean = node match {

    case node: ImplicitVar ⇒ true
    case t: Term if t.args.collectFirst {
      case arg: Term if scan(arg) ⇒ true
    }.getOrElse(false) ⇒ true
    case t: Term if t.optargs.collectFirst {
      case p: com.rethinkscala.AssocPair if scan(p.token) ⇒ true
    }.getOrElse(false) ⇒ true
    case _ ⇒ false
  }

  def apply(): Term = {
    val expressed = Expr(value)
    expressed match {
      case f: Func => f
      case _ => if (scan(expressed)) new ScalaPredicate1((v: Var) ⇒ expressed.asInstanceOf[Typed]).apply() else expressed
    }
  }
}

private[rethinkscala] case class MakeObj2(data: AnyRef) extends Term with MapTyped {

  override protected val extractArgs = false
  override lazy val optargs = buildOptArgs2(Expr.mapForInsert(data))

  def termType: TermType = TermType.MAKE_OBJ
}

object MakeObj {

  def asJson(data: Map[String, Any], depth: Int = 20): MakeObj = new MakeObj(data) {
    override lazy val optargs = buildOptArgs2(data.mapValues(v ⇒ Expr.json(v, depth)))
  }
}

case class MakeObj(data: Map[String, Any]) extends Term with MapTyped {

  override protected val extractArgs = false
  override lazy val optargs = buildOptArgs2(data)

  def termType: TermType = TermType.MAKE_OBJ
}

case class Var(id: Int) extends ProduceAny {
  override lazy val args = buildArgs(id)

  def termType: TermType = TermType.VAR

  override private[rethinkscala] def print(args: Seq[String], opt: Map[String, String]) = s"var_${args(0)}"
}

case class JavaScript(code: String, timeout: Option[Int] = None) extends TopLevelQuery {
  override lazy val args = buildArgs(code)

  override lazy val optargs = buildOptArgs(Map("timeout" -> timeout))

  override val stmt = "js"

  def termType: TermType = TermType.JAVASCRIPT
}

case class Range(start: Int, end: Option[Int] = None) extends ProduceDefaultSequence[Int] {

  override lazy val args = buildArgs(end.map(e => Seq(start, e)).getOrElse(Seq(start)): _*)

  override def termType = TermType.RANGE
}


case class UserError(error: String) extends TopLevelQuery {

  def termType: TermType = TermType.ERROR

  override val stmt = "error"
}

class ImplicitVar extends ProduceAny {

  def termType: TermType = TermType.IMPLICIT_VAR


  override private[rethinkscala] def print(args: Seq[String], opt: Map[String, String]) = "r.row"
}

case class Info(target: Typed) extends ProduceDocument[InfoResult] {
  def termType: TermType = TermType.INFO
}

case class Branch(test: Binary, passed: Typed, failed: Typed) extends ProduceAny {

  def termType: TermType = TermType.BRANCH
}

object Core {
  val row = new ImplicitVar()
}

/**
 * Loop over a sequence, evaluating the given write query for each element.
 * @param target
 * @param function
 */
case class ForEach[T, C[_]](target: Sequence[T, C], function: Predicate1) extends ProduceAnyDocument {

  override lazy val args = buildArgs(target, FuncWrap(function))

  def termType: TermType = TermType.FOR_EACH
}

case class CoerceTo(target: Typed, dataType: DataType) extends ProduceAny {

  override lazy val args = buildArgs(target, dataType.name)

  def termType: TermType = TermType.COERCE_TO
}

case class FuncCall(function: Predicate, values: Seq[Typed]) extends ProduceAny {

  override lazy val args = buildArgs(values.+:(function()): _*)

  def termType: TermType = TermType.FUNCALL
}


object internal {

  case class Continue[T](token: Long) extends ProduceDefaultSequence[T] {
    override def termType = null
  }

}


trait Expr {
  def apply(term: Term): Term = term

  def apply[T](value: Iterable[T]) = MakeArray(value)

  def apply(value: Map[String, Any]): MakeObj = MakeObj(value)

  def apply(value: String): StringDatum = StringDatum(value)

  def apply(b: Boolean): BooleanDatum = BooleanDatum(b)

  def apply(i: Int): NumberDatum = NumberDatum(i)

  def apply(l: Long): NumberDatum = NumberDatum(l)

  def apply(f: Float): NumberDatum = NumberDatum(f)

  def apply(f: Double): NumberDatum = NumberDatum(f)

  def apply(d: Document) = MakeObj2(d)

  def apply(v: WrappedValue[_]): Term = apply(v.value)

  def apply(date: ReadableInstant) = ISO8601(ISODateTimeFormat.dateTime().print(date))

  private def canInsert(name: String, value: AnyRef, writeNulls: Boolean): Boolean = {
    val nullType = value == None || value == null
    val id = name == "id"

    if (id) return !nullType
    nullType && writeNulls || !nullType

  }

  private[rethinkscala] def mapForInsert(doc: AnyRef): Map[String, Any] = Reflector.toMap(doc)


  def apply(a: Any, depth: Int = 20, writeNulls: Boolean = false): Term = {
    if (depth < 0) throw RethinkDriverError("Nesting depth limit exceeded")

    a match {
      case w: FuncWrap ⇒ w()
      case wv: WrappedValue[_] ⇒ apply(wv.value, depth)
      case wt: WrappedTerm ⇒ wt.unwrap
      case date: ReadableInstant ⇒ apply(date)
      case p: Predicate ⇒ p()
      case t: Term ⇒ t
      case f: Any if !f.isInstanceOf[Iterable[_]] && f.isInstanceOf[OfFunction1] ⇒ new ScalaPredicate1(f.asInstanceOf[OfFunction1]).apply()
      case f: Any if !f.isInstanceOf[Iterable[_]] && f.isInstanceOf[OfFunction2] ⇒ new ScalaPredicate2(f.asInstanceOf[OfFunction2]).apply()
      case s: Seq[_] ⇒ MakeArray(s, depth - 1)
      case m: Map[_, _] ⇒ MakeObj(m.asInstanceOf[Map[String, Any]])
      case Some(a: Any) ⇒ apply(a, depth - 1)
      case InsertExpr(value) ⇒ apply(value._1, depth - 1, value._2)
      case d: Document ⇒ MakeObj2(d)
      case c: Character ⇒ StringDatum(c.toString)
      case None | null ⇒ NoneDatum()
      case s: String ⇒ s match {
        case ISO8601Serializer(date) => date
        case _ => StringDatum(s)
      }
      case i: Int ⇒ NumberDatum(i)
      case f: Float ⇒ NumberDatum(f)
      case l: Long ⇒ NumberDatum(l)
      case b: Boolean ⇒ BooleanDatum(b)
      case d: Double ⇒ NumberDatum(d)
      case a: AnyRef ⇒ MakeObj2(a)
      case _ ⇒ throw RethinkDriverError(s"Can not determine rethink datatype : ${a.getClass.getName}")

    }
  }

  private[this] val DocumentClass = classOf[Document]

  private[this] val ReadableInstantClass = classOf[ReadableInstant]

  private[this] def isJsonClass(d: Class[_], depth: Int): Boolean = {
    Reflector.fields(d).find(f ⇒
      if (DocumentClass isAssignableFrom f.getType) isJsonClass(f.getType, depth - 1)
      else ReadableInstantClass isAssignableFrom f.getType).isEmpty

  }

  type OfMap = Map[String, _]
  type OfFunction1 = (Var) => Typed
  type OfFunction2 = (Var, Var) ⇒ Typed

  def isJson(v: Any, depth: Int = 20): Boolean = {
    if (depth < 0) throw RethinkDriverError("Nesting depth limit exceeded")

    v match {
      case t: Term ⇒ false
      case m: Map[String, _] ⇒ m.find {
        case (a, b) ⇒ !isJson(b, depth - 1)
      }.isEmpty
      case d: Document ⇒ isJsonClass(d.getClass, depth)
      case l: Seq[Any] ⇒ l.find(r ⇒ !isJson(r, depth - 1)).isEmpty
      case Int | Float | Boolean | Double | Long ⇒ true
      case a: Any if classOf[java.io.Serializable].isAssignableFrom(a.getClass) && a.getClass.getName.startsWith("java.lang") ⇒ true
      case _ ⇒ false

    }
  }

  def json(array: Seq[Any], depth: Int): Seq[Any] = {
    if (depth < 0) throw RethinkDriverError("Nesting depth limit exceeded")
    array.map(v ⇒ json(v, depth - 1)).toSeq
  }

  def json(v: Any, depth: Int = 20): Term = {
    if (depth < 0) throw RethinkDriverError("Nesting depth limit exceeded")
    v match {
      case t: Term ⇒ t
      case a: Any if isJson(a, depth) ⇒ Json.asLazy(a)

      case d: Map[String, _] ⇒ MakeObj.asJson(d, depth)
      case l: Iterable[Any] ⇒ MakeArray.asJson(l, depth)
      case a: Any ⇒ Expr(a)
    }

  }
}

trait WrappedValue[T] {
  val value: T
}

trait WrappedTerm {
  def unwrap: Term
}

object Expr extends Expr

trait JsonLike

case class LazyJson(value: Any) extends Produce[JsonDocument] with JsonDocumentConversion with JsonLike {

  override lazy val args = buildArgs(Reflector.toJson(value))

  def termType: TermType = TermType.JSON
}

case class Json(value: String) extends Produce[JsonDocument] with JsonDocumentConversion with JsonLike {
  def termType: TermType = TermType.JSON
}

class Default[T](target: Typed, defaultValue: T) extends MethodQuery {


  override lazy val args = buildArgs(target, defaultValue)

  def termType: TermType = TermType.DEFAULT
}

object Json {
  def asLazy(value: Any) = LazyJson(value)
}

class Random[@specialized(Int, Double, Long) T, R](val values: Seq[T], float: Option[Boolean] = None) extends Query {

  override lazy val args = buildArgs(values: _*)

  override lazy val optargs = buildOptArgs(Map("float" -> float))

  def termType: TermType = TermType.RANDOM
}

final class RandomSupport[T, R](val target: Random[T, R]) extends AnyVal {
  def toFloat = new Random[T, Float](target.values, Some(true)) with ProduceFloat
}

object Random {

  def apply[T](values: Seq[T]) = new Random[T, T](values) with ProduceNumeric

}

class UUID extends Produce[java.util.UUID] {
  self ⇒
  override def termType: TermType = TermType.UUID

  def string = new ProduceString {

    override private[rethinkscala] val underlyingTerm: Term = self

    override def termType: TermType = TermType.UUID

  }
}
