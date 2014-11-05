package com.rethinkscala

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 11/4/2014
 * Time: 5:05 PM 
 */


case class ConnectionPoolMsg(
                          userId: String,
                          someId: String,
                          totalPools: Int,
                          hello: Double,
                          list: List[ConnectionPool],
                          time: Long
                          ) extends Document

object ConnectionPoolMsg {
  // Random generator
  val random = new scala.util.Random

  // Generate a random alphabnumeric string of length n
  def string(n: Int) =
    randomString("abcdefghijklmnopqrstuvwxyz0123456789")(n)

  def randomString(alphabet: String)(n: Int): String =
    Stream.continually(random.nextInt(alphabet.size)).map(alphabet).take(n).mkString

  def create = ConnectionPoolMsg(
    string(5),
    string(10),
    random.nextInt(50),
    random.nextDouble(),
    List(
      ConnectionPool(string(5),
        random.nextDouble(),
        PoolProvider(string(6), string(6), Seq(string(8)), Map(string(5) -> DayHours(1, 5)),
          LALA(string(6), string(4), Some(string(6)), Some(Seq(string(6)))),
          Location(string(6), None, string(5), string(4), string(6), string(3), string(3)),
          GeoLocation(string(5), Seq(1, 3, 6, 3))
        ))),
    random.nextLong())


}

case class ConnectionPool(
                `type`: String,
                distance: Double,
                obj: PoolProvider
                )

case class PoolProvider(
                        id: String,
                        additionalInfo: String,
                        number: Seq[String],
                        hour: Map[String, DayHours],
                        lala: LALA,
                        location: Location,
                        geo: GeoLocation
                        )

case class DayHours(
                     from: Int,
                     to: Int
                     )

case class LALA(
                     id: String,
                     name: String,
                     logo: Option[String],
                     all: Option[Seq[String]]
                     )

case class Location(
                    line1: String,
                    line2: Option[String],
                    suburb: String,
                    city: String,
                    state: String,
                    postcode: String,
                    country: String
                    )

case class GeoLocation(
                        `type`: String,
                        coordinates: Seq[Double]
                        )



