package com.rethinkscala.ast

import com.rethinkscala._


/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 2/24/14
 * Time: 8:13 AM 
 */

trait Array[T] extends Typed {
  override val underlying = this

}

object ArrayTyped {
  implicit def mapDocumentToSequence[T <: Document, ST >: Var, S[ST] <: Array[ST]](implicit fa: FromAst[ST]) = CanMap[T, S[ST], fa.Raw]
}

trait ArrayTyped[T] extends Array[T] {

  override val underlying = this

  def append(value: T) = Append(underlying, value)

  def prepend[B >: T](value: B) = Prepend(underlying, value)

  def diff[B >: T](values: Iterable[B]) = Difference(underlying, values)

  def diff[B >: T](value: ArrayTyped[B]) = Difference(underlying, value)

  def idiff[B >: T](array: ArrayTyped[B]) = Difference(array, underlying)

  def setInsert[B >: T](value: B) = SetInsert(underlying, value)

  def setUnion[B >: T](values: Iterable[B]) = SetUnion(underlying, values)

  def setUnion[B >: T](value: ArrayTyped[B]) = SetUnion(underlying, value)

  def setIntersection[B >: T](values: Iterable[B]) = SetIntersection(underlying, values)

  def setIntersection[B >: T](value: ArrayTyped[B]) = SetIntersection(underlying, value)

  def setDifference[B >: T](values: Iterable[B]) = SetDifference(underlying, values)

  def setDifference[B >: T](values: ArrayTyped[B]) = SetDifference(underlying, values)

  def insertAt[B >: T](index: Int, value: B) = InsertAt(underlying, index, value)

  def spliceAt(index: Int, values: T*) = SpliceAt(underlying, index, values)

  def deleteAt(start: Int, end: Option[Int] = None) = DeleteAt(underlying, start, end)

  def changeAt[B >: T](index: Int, value: B) = ChangeAt(underlying, index, value)

  def :+(value: T) = append(value)

  def ::[B >: T](value: T) = prepend(value)

  def +:(value: T) = prepend(value)

  def |+[B >: T](value: B) = setInsert(value)

  def ||[B >: T](values: Iterable[B]) = setUnion(values)

  def ||[B >: T](value: ArrayTyped[B]) = setUnion(value)

  def /\[B >: T](values: Iterable[B]) = setIntersection(values)

  def /\[B >: T](value: ArrayTyped[B]) = setIntersection(value)

  def \/[B >: T](values: Iterable[B]) = setDifference(values)

  def \/[B >: T](value: ArrayTyped[B]) = setDifference(value)

}