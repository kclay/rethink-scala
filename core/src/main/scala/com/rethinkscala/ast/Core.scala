package com.rethinkscala.ast

import com.rethinkscala._

import ql2.Ql2.Term.TermType
import com.rethinkscala.reflect.Reflector
import org.joda.time.ReadableInstant
import org.joda.time.format.ISODateTimeFormat
import com.rethinkscala.net.{JsonDocumentConversion, RethinkDriverError}
import scala.collection.{mutable, Iterable}

case class MakeArray[T](array: Iterable[T]) extends  ProduceArray[T] {
  override lazy val args = buildArgs(array.toSeq: _*)

  // def datumTermType:TermType.EnumVal=ql2.Datum
  def termType = TermType.MAKE_ARRAY

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
    val rtn = if (scan(value)) new ScalaPredicate1((v: Var) => e.asInstanceOf[Typed]).apply() else e
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

 object MakeObj {


  def asJson(data: Map[String, Any], depth: Int = 20): MakeObj = new MakeObj(data) {
    override lazy val optargs = buildOptArgs2(data.mapValues(v => Expr.json(v, depth)))
  }
}

case class MakeObj(data: Map[String, Any]) extends Term with MapTyped {

  override protected val extractArgs = false
  override lazy val optargs = buildOptArgs2(data)

  def termType = TermType.MAKE_OBJ
}

case class Var(id: Int) extends ProduceAny {
  override lazy val args = buildArgs(id)

  def termType = TermType.VAR

  override private[rethinkscala] def print(args: Seq[String], opt: Map[String, String]) = s"var_${args(0)}"
}

case class JavaScript(code: String, timeout: Option[Int] = None) extends TopLevelQuery {
  override lazy val args = buildArgs(code)

  override lazy val optargs = buildOptArgs(Map("timeout" -> timeout))


  override val stmt = "js"

  def termType = TermType.JAVASCRIPT
}


case class Default(value: Any) extends MethodQuery {

  def termType = TermType.DEFAULT
}

case class UserError(error: String) extends TopLevelQuery {

  def termType = TermType.ERROR

  override val stmt = "error"
}

class ImplicitVar extends ProduceAny {

  def termType = TermType.IMPLICIT_VAR

  // def apply(name: String) = this field name

  override private[rethinkscala] def print(args: Seq[String], opt: Map[String, String]) = "r.row"
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
case class ForEach[T,C[_]](target: Sequence[T,C], function: Predicate1) extends ProduceAnyDocument {

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


trait Expr {
  def apply(term: Term): Term = term

  def apply[T](value: Iterable[T]) = MakeArray(value)

  //def apply(value: Seq[Any]) = MakeArray(value)

  def apply(value: Map[String, Any]): MakeObj = MakeObj(value)

  def apply(value: String): StringDatum = StringDatum(value)

  def apply(b: Boolean): BooleanDatum = BooleanDatum(b)

  def apply(i: Int): NumberDatum = NumberDatum(i)

  def apply(l: Long): NumberDatum = NumberDatum(l)

  def apply(f: Float): NumberDatum = NumberDatum(f)

  def apply(f: Double): NumberDatum = NumberDatum(f)

  def apply(d: Document) = MakeObj2(d)

  def apply(date: ReadableInstant) = ISO8601(ISODateTimeFormat.dateTime().print(date))


  def apply(a: Any, depth: Int = 20): Term = {
    if (depth < 0) throw RethinkDriverError("Nesting depth limit exceeded")

    Seq
    a match {
      case w: FuncWrap => w()


      case date: ReadableInstant => apply(date)


      case p: Predicate => p()
      case t: Term => t

      case f:Any if (!f.isInstanceOf[Iterable[_]] && f.isInstanceOf[OfFunction1]) => new ScalaPredicate1(f.asInstanceOf[OfFunction1]).apply()
      case f:Any if (!f.isInstanceOf[Iterable[_]] && f.isInstanceOf[OfFunction2]) => new ScalaPredicate2(f.asInstanceOf[OfFunction2]).apply()
      case s: Seq[_] => MakeArray(s, depth - 1)
      case m: Map[_, _] => MakeObj(m.asInstanceOf[Map[String, Option[Any]]])
      case d: Document => MakeObj2(d)

      case a: Any => Datum(a)


    }
  }


  private[this] val DocumentClass = classOf[Document]

  private[this] val ReadableInstantClass = classOf[ReadableInstant]

  private[this] def isJsonClass(d: Class[_], depth: Int): Boolean = {
    Reflector.fields(d).find(f =>
      if (DocumentClass isAssignableFrom f.getType) isJsonClass(f.getType, depth - 1)
      else ReadableInstantClass isAssignableFrom f.getType).isEmpty


  }

  type OfMap = Map[String, _]
  type OfFunction1 = (Var) => Typed
  type OfFunction2 = (Var, Var) => Typed

  def isJson(v: Any, depth: Int = 20): Boolean = {
    if (depth < 0) throw RethinkDriverError("Nesting depth limit exceeded")

    v match {
      case t: Term => false
      case m: Map[String, _] => m.find {
        case (a, b) => !isJson(b, depth - 1)
      }.isEmpty
      case d: Document => isJsonClass(d.getClass, depth)
      case l: Seq[Any] => l.find(r => !isJson(r, depth - 1)).isEmpty
      case Int | Float | Boolean | Double | Long => true

      case a: Any if classOf[java.io.Serializable].isAssignableFrom(a.getClass) && a.getClass.getName.startsWith("java.lang") => true

      // case d: Any if classOf[java.io.Serializable].isAssignableFrom(d.getClass) => throw RethinkDriverError("Use of classes must extend type Document")
      case _ => false

    }
  }

  def json(array: Seq[Any], depth: Int): Seq[Any] = {
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

object Expr extends Expr


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

class Random[T,R](values:Seq[T],float:Option[Boolean] = None) extends Query{


  override lazy val args = buildArgs(values:_*)

  def toFloat =new Random[T,Float](values,Some(true)) with ProduceFloat

  override lazy val optargs = buildOptArgs(Map("float"->float))

  def termType = TermType.RANDOM
}

object Random{

  def apply[T](values:Seq[T]) = new Random[T,T](values) with ProduceNumeric
}

