package com.rethinkscala.ast


/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 2/24/14
 * Time: 8:13 AM 
 */

trait Array extends Typed

trait ArrayTyped[T] extends Sequence[T] with Array {


  override val underlying = this

  def append(value: Datum) = Append(underlying, value)

  def :+(value: Datum) = append(value)

  def prepend(value: Datum) = Prepend(underlying, value)

  def +:(value: Datum) = prepend(value)

  def diff(values: Datum*) = Difference(underlying, Expr(values))

  def diff(array: ArrayTyped[_]) = Difference(underlying, array)

  def idiff(array: ArrayTyped[_]) = Difference(array, underlying)

  def idiff(values: Datum*) = Difference(Expr(values), underlying)


  def setInert(value: T) = SetInsert(underlying, value)

  def setUnion(values: T*) = SetUnion(underlying, values)

  def setIntersection(values: T*) = SetIntersection(underlying, values)


  def setDifference(values: T*) = SetDifference(underlying, values)

  def insertAt(index: Int, value: T) = InsertAt(underlying, index, value)

  def spliceAt(index: Int, values: T*) = SpliceAt(underlying, index, values)

  def deleteAt(start: Int, end: Option[Int] = None) = DeleteAt(underlying, start, end)

  def changeAt(index: Int, value:T) = ChangeAt(underlying, index, value)

}