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




  implicit def term2DataNum(t:Term):Datum= t.asInstanceOf[Datum]
  def ==(other: Datum) = Eq(this, other)

  def !=(other: Datum) = Ne(this, other)

  def <(other: Datum) = Lt(this, other)

  def <=(other: Datum) = Le(this, other)

  def >(other: Datum) = Gt(this, other)

  def >=(other: Datum) = Ge(this, other)

  def ~(other: Datum) = Not(this)

  def +(other: Datum) = Add(this, other)

  def >+(other: Datum) = Add(other, this)

  def -(other: Datum) = Sub(this, other)

  def >-(other: Datum) = Sub(other, this)

  def *(other: Datum) = Mul(this, other)

  def >*(other: Datum) = Mul(other, this)

  def /(other: Datum) = Div(this, other)

  def >/(other: Datum) = Div(other, this)

  def %(other: Datum) = Mod(this, other)

  def >%(other: Datum) = Mod(other, this)

  def &(other: Datum) = All(this, other)

  def &&(other: Datum) = All(other, this)

  def &>(other: Datum) = this && other

  // or
  def |(other: Datum) = RAny(this, other)

  // right or
  def >|(other: Datum) = RAny(other, this)
  def contains(attribute: String*) = Contains(this, attribute )
  //slize http://stackoverflow.com/questions/3932582/slice-notation-in-scala

}