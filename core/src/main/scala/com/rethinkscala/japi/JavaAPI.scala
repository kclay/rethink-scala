package com.rethinkscala.japi

import com.rethinkscala.ast.{Predicate => SPredicate, BooleanPredicate => BP, _}
import scala.util.control.NoStackTrace
import scala.runtime.AbstractPartialFunction
import java.util.Collections.{emptyList, singletonList}
import com.rethinkscala.{RethinkApi, Document, Term}
import com.rethinkscala.ast.StringDatum
import com.rethinkscala.ast.Var
import com.rethinkscala.ast.BooleanDatum
import com.rethinkscala.ast.NumberDatum
import org.joda.time.ReadableInstant


/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 9/13/13
 * Time: 2:09 AM
 *
 */


/**
 * A Function interface. Used to create first-class-functions is Java.
 */
trait Function[T, R] {
  @throws(classOf[Exception])
  def apply(param: T): R

}

/**
 * A Function interface. Used to create 2-arg first-class-functions is Java.
 */
trait Function2[T1, T2, R] {

  @throws(classOf[Exception])
  def apply(arg1: T1, arg2: T2): R
}


object JavaPartialFunction {

  sealed abstract class NoMatchException extends RuntimeException with NoStackTrace

  case object NoMatch extends NoMatchException

  final def noMatch(): RuntimeException = NoMatch
}

abstract class JavaPartialFunction[A, B] extends AbstractPartialFunction[A, B] {

  import JavaPartialFunction._

  @throws(classOf[Exception])
  def apply(x: A, isCheck: Boolean): B

  final def isDefinedAt(x: A): Boolean = try {
    apply(x, true); true
  } catch {
    case NoMatch ⇒ false
  }

  final override def apply(x: A): B = try apply(x, false) catch {
    case NoMatch ⇒ throw new MatchError(x)
  }

  final override def applyOrElse[A1 <: A, B1 >: B](x: A1, default: A1 ⇒ B1): B1 = try apply(x, false) catch {
    case NoMatch ⇒ default(x)
  }
}

/**
 * This class represents optional values. Instances of <code>Option</code>
 * are either instances of case class <code>Some</code> or it is case
 * object <code>None</code>.
 */
sealed abstract class Option[A] extends java.lang.Iterable[A] {
  def get: A

  def isEmpty: Boolean

  def isDefined: Boolean = !isEmpty

  def asScala: scala.Option[A]

  def iterator: java.util.Iterator[A] = if (isEmpty) emptyList[A].iterator else singletonList(get).iterator
}

object Option {
  /**
   * <code>Option</code> factory that creates <code>Some</code>
   */
  def some[A](v: A): Option[A] = Some(v)

  /**
   * <code>Option</code> factory that creates <code>None</code>
   */
  def none[A] = None.asInstanceOf[Option[A]]

  /**
   * <code>Option</code> factory that creates <code>None</code> if
   * <code>v</code> is <code>null</code>, <code>Some(v)</code> otherwise.
   */
  def option[A](v: A): Option[A] = if (v == null) none else some(v)

  /**
   * Converts a Scala Option to a Java Option
   */
  def fromScalaOption[T](scalaOption: scala.Option[T]): Option[T] = scalaOption match {
    case scala.Some(r) ⇒ some(r)
    case scala.None ⇒ none
  }

  /**
   * Class <code>Some[A]</code> represents existing values of type
   * <code>A</code>.
   */
  final case class Some[A](v: A) extends Option[A] {
    def get: A = v

    def isEmpty: Boolean = false

    def asScala: scala.Some[A] = scala.Some(v)
  }

  /**
   * This case object represents non-existent values.
   */
  private case object None extends Option[Nothing] {
    def get: Nothing = throw new NoSuchElementException("None.get")

    def isEmpty: Boolean = true

    def asScala: scala.None.type = scala.None
  }

  implicit def java2ScalaOption[A](o: Option[A]): scala.Option[A] = o.asScala

  implicit def scala2JavaOption[A](o: scala.Option[A]): Option[A] = if (o.isDefined) some(o.get) else none
}

trait Predicate extends SPredicate with Function[Var, Typed] {
  protected def _invoke(vars: Seq[Var]) = apply(vars(0))

  val amount: Int = 1
}

trait Predicate2 extends SPredicate with Function2[Var, Var, Typed] {
  protected def _invoke(vars: Seq[Var]) = apply(vars(0), vars(1))

  val amount: Int = 2
}


trait BooleanFunction extends BP with Function[Var, Binary] {
  protected def _invoke(vars: Seq[Var]) = apply(vars(0))

  val amount: Int = 1

}


trait BooleanFunction2 extends BP with Function2[Var, Var, Binary] {
  protected def _invoke(vars: Seq[Var]) = apply(vars(0), vars(1))

  val amount: Int = 2
}


object r extends RethinkApi {

  import scala.collection.JavaConversions._

  def expr(term: Term): Term = term

  def expr[T](value: java.util.Collection[T]): MakeArray[T] = Expr(value.toSeq)

  def expr(value: java.util.Map[String, Any]): MakeObj = Expr(value.toMap)

  def expr(value: String): StringDatum = Expr(value)

  def expr(value: Boolean): BooleanDatum = Expr(value)

  def expr(value: Int): NumberDatum = Expr(value)

  def expr(value: Long): NumberDatum = Expr(value)

  def expr(value: Float): NumberDatum = Expr(value)

  def expr(value: Double): NumberDatum = Expr(value)

  def expr(value: Document) = Expr(value)

  def expr(value: ReadableInstant) = Expr(value)
}

