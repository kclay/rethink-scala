package com.rethinkscala

import com.rethinkscala.ast._

import ql2.Term.TermType
import ql2.Datum.DatumType
import com.rethinkscala.relect.Reflector

trait AssocPair {

  type T
  val key: String
  val value: Any
  lazy val token = Expr(value)

  def pair: T

}

trait ExprWrap

trait WithAst {
  def ast: ql2.Term
}

trait Message[T] {

  def toMessage: T

}

trait TermMessage extends Message[ql2.Term] with Term {

  def toMessage: ql2.Term = ast
}

trait DatumMessage extends Message[ql2.Datum] with Term {
  def termType = TermType.DATUM

  def datumType: DatumType.EnumVal

  def build(d: ql2.Datum): ql2.Datum

  override def ast: ql2.Term = super.ast.setDatum(toMessage)

  def toMessage: ql2.Datum = build(ql2.Datum(Some(datumType)))
}

case class TermAssocPair(key: String, value: Any) extends AssocPair {
  type T = ql2.Term.AssocPair

  def pair = ql2.Term.AssocPair(Some(key), Some(token.ast))
}

case class DatumAssocPair(key: String, value: Any) extends AssocPair {
  type T = ql2.Datum.AssocPair

  def pair = ql2.Datum.AssocPair(
    Some(key), Some(token.asInstanceOf[Datum].toMessage))

}

trait Term extends WithAst {

  protected val extractArgs = true

  def ast = ql2
    .Term(Some(termType))
    .addAllArgs(args.map(_.ast))
    .addAllOptargs(optargs.map(_.pair.asInstanceOf[ql2.Term.AssocPair]))

  lazy val args: Seq[Term] =  if(extractArgs) buildArgs(Reflector.fields(this).map(_.get(this)): _*) else Seq.empty[Term]

  protected def buildArgs(args: Any*): Seq[Term] = for (a <- args) yield Expr(a)

  lazy val optargs = Iterable.empty[AssocPair]

  def optArgsBuilder(key: String, value: Any): AssocPair = TermAssocPair(key, value)

  protected def buildOptArgs2(optargs: Map[String, Any]): Iterable[AssocPair] = optargs.collect {
    case (key: String, value: Any) => optArgsBuilder(key, value)
  }
  protected def buildOptArgs(optargs: Map[String, Option[Any]]): Iterable[AssocPair] = optargs.filter(_._2.isDefined) collect {
    case (key: String, value: Option[Any]) => optArgsBuilder(key, value.get)
  }

  def termType: ql2.Term.TermType.EnumVal


}

