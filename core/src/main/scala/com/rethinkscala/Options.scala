package com.rethinkscala

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 8/17/13
 * Time: 9:59 AM
 *
 */
trait Options {
  def toMap: Map[String, Option[_]]
}


case class TableOptions(
                         durability: Option[Durability.Value] = None,
                         primaryKey: Option[String] = None,
                         cacheSize: Option[Int] = None,
                         dataCenter: Option[String] = None
                         ) extends Options {
  def toMap = Map("primary_key" -> primaryKey, "datacenter" -> dataCenter, "cache_size" -> cacheSize, "durability" -> durability)
}

case class QueryOptions(
                         useOutdated: Option[Boolean] = None,
                         noreply: Option[Boolean] = None
                         ) extends Options {
  def toMap = ???
}


case class InsertOptions(
                          durability: Option[Durability.Value] = None,
                          returnValues: Option[Boolean] = None,
                          upsert: Option[Boolean] = None
                          ) extends Options {
  def toMap = Map("upsert" -> upsert, "durability" -> durability, "return_vals" -> returnValues)
}


case class UpdateOptions(
                          nonAtomic: Option[Boolean] = None,
                          durability: Option[Durability.Value] = None,
                          returnValues: Option[Boolean] = None
                          ) extends Options {
  def toMap = Map("non_atomic" -> nonAtomic, "durability" -> durability, "return_vals" -> returnValues)
}



case class BoundOptions(
                         leftBound: Option[Bound.Value] = None,
                         rightBound: Option[Bound.Value] = None
                         ) extends Options {
  def toMap = Map("left_bound" -> leftBound, "right_bound" -> rightBound)
}

object Bound extends Enumeration {
  type Kind = Value
  val Open = Value("open")
  val Closed = Value("closed")
}

//case class DuringOptions

object Durability extends Enumeration {
  type Kind = Value
  val Hard = Value("hard")
  val Soft = Value("soft")
}