package com.rethinkscala.ast

import com.rethinkscala.{CanMap, FromAst, Document}
import com.rethinkscala.ast.Sequence


/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 2/24/14
 * Time: 8:13 AM 
 */

trait Array[T] extends Typed{
  override val underlying = this

}

object ArrayTyped{
  implicit def mapDocumentToSequence[T<:Document,ST>:Var,S[ST]<:Array[ST] ](implicit fa:FromAst[ST])= CanMap[T,S[ST],fa.Raw]

  implicit class ScalaArrayTyped[T](underlying:ArrayTyped[T]){
    def :+(value: T) = underlying.append(value)

    def ::[B>:T](value:T) = underlying.prepend(value)





    def +:(value: T) = underlying.prepend(value)
  }
}
trait ArrayTyped[T] extends Array[T] {


  override val underlying = this

  def append(value: T) = Append(underlying, value)

  def prepend[B >:T](value: B) = Prepend(underlying, value)


 def diff[B>:T](values:B*) = Difference(underlying,Expr(values))
 def diff[B >: T](value: ArrayTyped[B]) = Difference(underlying, value)

 // def diff(array: ArrayTyped[_]) = Difference(underlying, array)

  def idiff[B >: T](array: ArrayTyped[B]) = Difference(array, underlying)




  def setInsert[B >: T](value: B) = SetInsert(underlying, value)


  def setUnion[B >: T](values: B*) = SetUnion(underlying, values)
  def setUnion[B >: T](value: ArrayTyped[B]) = SetUnion(underlying, value)

  def setIntersection[B>:T](values: B*) = SetIntersection(underlying, values)
  def setIntersection[B>:T](values: ArrayTyped[B]) = SetIntersection(underlying, values)


  def setDifference[B>:T](values: B*) = SetDifference(underlying, values)

  def setDifference[B>:T](values: ArrayTyped[B]) = SetDifference(underlying, values)

  def insertAt[B>:T](index: Int, value: B) = InsertAt(underlying, index, value)

  def spliceAt(index: Int, values: T*) = SpliceAt(underlying, index, values)

  def deleteAt(start: Int, end: Option[Int] = None) = DeleteAt(underlying, start, end)

  def changeAt[B>:T](index: Int, value:B) = ChangeAt(underlying, index, value)

}