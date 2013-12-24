package com.rethinkscala

import com.rethinkscala.ast._

import ql2.{Ql2 => ql2}
import ql2.Term.TermType
import ql2.Datum.DatumType
import com.rethinkscala.reflect.Reflector

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

  def datumType: DatumType

  def build(d: ql2.Datum.Builder): ql2.Datum.Builder


  override protected def newBuilder = super.newBuilder.setDatum(toMessage)

  def toMessage: ql2.Datum = build(ql2.Datum.newBuilder.setType(datumType)).build()
}

case class TermAssocPair(key: String, value: Any) extends AssocPair {
  type T = ql2.Term.AssocPair

  def pair = ql2.Term.AssocPair.newBuilder.setKey(key).setVal(token.ast).build()
}

case class DatumAssocPair(key: String, value: Any) extends AssocPair {
  type T = ql2.Datum.AssocPair

  def pair = ql2.Datum.AssocPair.newBuilder.setKey(key).setVal(token.asInstanceOf[Datum].toMessage).build()


}

trait Term extends WithAst {




  import scala.collection.JavaConversions.{seqAsJavaList, asJavaCollection}

  protected val extractArgs = true

  protected def newBuilder = ql2.Term.newBuilder()

  def ast = newBuilder.setType(termType)
    .addAllArgs(args.map(_.ast))
    .addAllOptargs(optargs.map(_.pair.asInstanceOf[ql2.Term.AssocPair])).build()


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
      case e: Enumeration#Value => e.toString()
      case x: Any => x
    })
  }

  def termType: ql2.Term.TermType

}

