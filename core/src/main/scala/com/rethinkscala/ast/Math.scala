package com.rethinkscala.ast

import ql2.Ql2.Term.TermType
import com.rethinkscala.Term


case class Eq(left: Typed, right: Typed) extends BiCompareOperQuery with ProduceBinary {
  def termType = TermType.EQ

  override val stmt = "=="
}

case class Ne(left: Typed, right: Typed) extends BiCompareOperQuery with ProduceBinary {
  def termType = TermType.NE

  override val stmt = "!="
}

case class Lt(left: Typed, right: Typed) extends BiCompareOperQuery with ProduceBinary {
  def termType = TermType.LT

  override val stmt = "<"
}

case class Le(left: Typed, right: Typed) extends BiCompareOperQuery with ProduceBinary {
  def termType = TermType.LE

  override val stmt = "<="
}

case class Gt(left: Typed, right: Typed) extends BiCompareOperQuery with ProduceBinary {
  def termType = TermType.GT

  override val stmt = ">"
}

case class Ge(left: Typed, right: Typed) extends BiCompareOperQuery with ProduceBinary {
  def termType = TermType.GE

  override val stmt = ">="
}

case class Not(prev: Typed) extends Query {
  override lazy val args = buildArgs(prev)

  def termType = TermType.NOT

  /*
  override private[rethinkscala] def print(args: Seq[String], opt: Map[String, String]) = args(0) match{
    case d:Datum=>""
  } */
}

case class Sub(left: Numeric, right: Numeric) extends BiCompareOperQuery with ProduceNumeric {
  def termType = TermType.SUB
}

case class Mul(left: Multiply, right: Numeric) extends BiCompareOperQuery with ProduceNumeric {
  def termType = TermType.MUL
}

case class Div(left: Numeric, right: Numeric) extends BiCompareOperQuery with ProduceNumeric {
  def termType = TermType.DIV
}

case class Mod(left: Numeric, right: Numeric) extends BiCompareOperQuery with ProduceNumeric {
  def termType = TermType.MOD
}

abstract case class All(left:Typed, right: Typed) extends BiCompareOperQuery {

  override lazy val args = buildArgs(left,right)
  def termType = TermType.ALL
}

abstract case class Or(left: Typed, right: Typed) extends BiCompareOperQuery {

  override lazy val args = buildArgs(left,right )
  def termType = TermType.ANY
}


object All{
  def apply(left:Binary,right:Binary) = new All(left,right) with ProduceBinary

  def apply[T](left:Typed,right:ProduceSequence[T]) = new All(left,right) with ProduceSequence[T]
  def apply(left:Typed,right:Strings) = new All(left,right) with ProduceString
  def apply(left:Typed,right:Numeric) = new All(left,right) with ProduceNumeric
}



object Or{

  def apply(left:Binary,right:Binary) = new Or(left,right) with ProduceBinary
  def apply[L,R>:L](left:ProduceSequence[L],right:ProduceSequence[R]) =   new Or(left,right) with ProduceSequence[L]
}

abstract case class Add(left: Typed, right: Typed) extends BiCompareOperQuery  {


  override lazy val args = buildArgs(left,right)

  def termType = TermType.ADD
}


object Add{

  def any(left:Typed,right:Typed) = new Add(left,right) with ProduceAny

  def apply(left:Numeric,right:Numeric) = new Add(left,right) with ProduceNumeric
  def apply(left:Strings,right:Strings):ProduceString = new Add(left,right) with ProduceString

  def apply[L,R>:L](left:ProduceSequence[L],right:ProduceSequence[R]) =   new Add(left,right) with ProduceSequence[L]
}


//case class AnyAdd(left: Addition, right: Addition) extends Add with ProduceAny


//case class NumericAdd(left: Addition, right: Addition) extends Add with ProduceNumeric

//case class StringAdd(left: Addition, right: Addition) extends Add with ProduceString