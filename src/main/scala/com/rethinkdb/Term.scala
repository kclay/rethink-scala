package com.rethinkdb

import ql2.{Ql2=>p}
import com.rethinkdb.ast._
import com.rethinkdb.conversions.Tokens._


trait Composable {
  def compose(args: Seq[Term], optargs: Map[String, Term]) = {
    ""
  }
}

trait BiOpTerm extends Term with Composable

trait TopLevelTerm extends Term with Composable

trait MethodTerm extends Term with Composable

trait ExprWrap

trait Produce[T]{
  def produced:T
}

trait ProducesBoolean extends Produce[Boolean]



trait Term extends TermBlock {

  def ast: String = "Term"

  lazy val args = Seq.empty[Term]


  protected def buildArgs(args: Any*) = for (a <- args) yield Expr(a)

  lazy val optargs = Iterable.empty[AssocPairToken]


  protected def buildOptArgs(optargs: Map[String, Option[Any]]): Iterable[AssocPairToken] = optargs.filter(_._2.isDefined) collect {
    case (key: String, value: Option[Any]) => optArgsBuilder(key, value.get)
  }

  def optArgsBuilder(key: String, value: Any): AssocPairToken = TermAssocPairToken(key, value)


  def termType: TokenType


  def !(connection: Connection) = {
    connection ? this
  }

  def run() = {

    None
  }


  type TermType = p.Term.TermType

  def toTerm={
    val builder = p.Term.newBuilder()
    compile(builder)
    builder.build()
  }

  def compile(builder: p.Term.Builder):Unit = {

    builder.setType(termType.value.asInstanceOf[TermType])
    for (a <- args) builder.addArgs(a)
    for (o <- optargs) builder.addOptargs(o)
    // builder.build()


  }

  def ==(other: Datum) = Eq(this, other)

  def !=(other: Datum) = Ne(this, other)

  def <(other: Datum) = Lt(this, other)

  def <=(other: Datum) = Le(this, other)

  def >(other: Datum) = Gt(this, other)

  def >=(other: Datum) = Ge(this, other)

  def ~(other: Datum) = Not(this)

  def +(other: Term) = Add(this, other)

  def >+(other: Term) = Add(other, this)

  def -(other: Term) = Sub(this, other)

  def >-(other: Term) = Sub(other, this)

  def *(other: Term) = Mul(this, other)

  def >*(other: Term) = Mul(other, this)

  def /(other: Term) = Div(this, other)

  def >/(other: Term) = Div(other, this)

  def %(other: Term) = Mod(this, other)

  def >%(other: Term) = Mod(other, this)

  def &(other: Term) = All(this, other)

  def &&(other: Term) = All(other, this)

  def &>(other: Term) = this && other

  // or
  def |(other: Term) = RAny(this, other)

  // right or
  def >|(other: Term) = RAny(other, this)

  def contains(attribute: String*) = Contains(this, attribute )
  //slize http://stackoverflow.com/questions/3932582/slice-notation-in-scala

}