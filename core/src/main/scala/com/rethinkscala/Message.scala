package com.rethinkscala

import com.rethinkscala.ast._

import ql2.{Ql2 => ql2}
import ql2.Term.TermType
import ql2.Datum.DatumType
import com.rethinkscala.reflect.Reflector
import com.rethinkscala.net.Connection

trait AssocPair {


  val key: String
  val value: Any
  lazy val token = Expr(value)

  // def pair: T

}

trait ExprWrap

trait WithAst {

}

trait Message[T] {

  def toMessage: T

}

trait DatumMessage extends  Term {
  def termType = TermType.DATUM

  def datumType: DatumType

  def build(d: ql2.Datum.Builder): ql2.Datum.Builder


  override private[rethinkscala] def newBuilder = super.newBuilder.setDatum(toMessage)

  def toMessage: ql2.Datum = build(ql2.Datum.newBuilder.setType(datumType)).build()
}

case class TermAssocPair(key: String, value: Any) extends AssocPair

case class DatumAssocPair(key: String, value: Any) extends AssocPair

trait Term extends WithAst {


  private[rethinkscala] val underlyingTerm: Term = this

  import scala.collection.JavaConversions.{seqAsJavaList, asJavaCollection}

  protected val extractArgs = true

  private[rethinkscala] def newBuilder = ql2.Term.newBuilder()

  /*
  def ast = newBuilder.setType(termType)
    .addAllArgs(args.map(_.ast))
    .addAllOptargs(optargs.map(_.pair.asInstanceOf[ql2.Term.AssocPair])).build()

  */
  def ast(implicit connection: Connection) = connection toAst underlyingTerm

  lazy val args: Seq[Term] = if (extractArgs) buildArgs(Reflector.fields(this).map(_.get(this)): _*) else Seq.empty[Term]

  protected def buildArgs(args: Any*): Seq[Term] = for (a <- args) yield Expr(a)

  protected def buildArgs2(depth: Int, args: Any*): Seq[Term] = for (a <- args) yield Expr(a, depth)

  lazy val optargs = Iterable.empty[AssocPair]

  def optArgsBuilder(key: String, value: Any): AssocPair = TermAssocPair(key, value)

  protected def buildOptArgs2(optargs: Map[String, Any]): Iterable[AssocPair] = optargs.collect {
    case (key: String, value: Any) => optArgsBuilder(key, value)
  }

  protected def buildOptArgs(optargs: Map[String, Option[Any]]): Iterable[AssocPair] = optargs.filter(_._2.isDefined) collect {
    case (key: String, value: Some[Any]) => optArgsBuilder(key, value.get match {
      case e: Enumeration#Value => e.toString
      case wv: WrappedValue[_] => wv.value
      case x: Any => x
    })
  }

  def termType: ql2.Term.TermType

}

