package com.rethinkscala.ast

sealed trait AggregateByMethod {
  val underlying: Map[String, Any]
}

/** Count the total size of the group.
 */
case object ByCount extends AggregateByMethod {
  val underlying = Map("COUNT" -> true)
}

/** Compute the sum of the given field in the group.
 *  @param attr
 */
case class BySum(attr: String) extends AggregateByMethod {
  val underlying = Map("SUM" -> attr)
}

/** Compute the average value of the given attribute for the group.
 *  @param attr
 */
case class ByAvg(attr: String) extends AggregateByMethod {
  val underlying = Map("AVG" -> attr)
}