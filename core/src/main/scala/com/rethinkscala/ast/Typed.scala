package com.rethinkscala.ast

import com.rethinkscala._


sealed trait DataType {

  def name: String
}

case object ObjectData extends DataType {
  def name = "object"
}

case object StringData extends DataType {
  def name = "string"
}

case object ArrayData extends DataType {
  def name = "array"
}


private[rethinkscala] trait Typed extends ImplicitConversions {

  // TODO : Fix me
  def term: Term = this.asInstanceOf[Term]

  val underlying = this

  def info = Info(underlying)

  def typeOf = TypeOf(underlying)

  def coerceTo(dataType: DataType) = CoerceTo(underlying, dataType)
}


trait Ref extends ArrayTyped[Any] with Numeric with Binary with Record with Literal with Strings {
  override val underlying = this
}

trait JoinTyped[L, R] extends Typed {
  override val underlying = this

  def zip = Zip(underlying)
}


trait Multiply extends Typed {

  override val underlying = this

  def *(other: Numeric): Mul = mul(other)

  def *(other: Double): Mul = mul(other)

  def mul(other: Numeric): Mul = Mul(underlying, other)

  def mul(other: Double): Mul = Mul(underlying, other)
}


trait Binary extends Typed {

  override val underlying = this

  def &(other: Binary) = and(other)

  def and(other: Binary) = All(underlying, other)

  def rand(other: Binary) = All(other, underlying)

  def &>(other: Binary) = rand(other)

  // or
  def ||(other: Binary) = or(other)

  def or(other: Binary) = Or(underlying, other)

  // right or
  def >|(other: Binary) = ror(other)

  def ror(other: Binary) = Or(other, underlying)
}


sealed trait LogicSignature
