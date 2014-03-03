package com.rethinkscala.ast

import org.joda.time.DateTime
import com.rethinkscala.BoundOptions

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 2/24/14
 * Time: 8:20 AM 
 */

trait TimeTyped extends Literal with ProduceSingle[DateTime] {
  implicit def dateTimeToTimeTyped(dt: DateTime) = Expr(dt)

  override val underlying = this

  def inTimeZone(timezone: String): InTimeZone = InTimeZone(underlying, Right(timezone))

  def inTimeZone(time: TimeTyped): InTimeZone = InTimeZone(underlying, Left(time))

  def timezone = Timezone(underlying)

  def during(start: DateTime, end: DateTime): During = During(underlying, start, end, BoundOptions())

  def during(start: DateTime, end: DateTime, bounds: BoundOptions): During = During(underlying, start, end, bounds)

  //def during(start: TimeTyped, end: TimeTyped, bounds: Option[BoundOptions] = None):During = During(underlying, start, end, bounds)
  def date = Date(underlying)

  def day = Day(underlying)

  def timeOfDay = TimeOfDay(underlying)

  def year = Year(underlying)

  def month = Month(underlying)

  def dayOfWeek = DayOfWeek(underlying)

  def dayOfYear = DayOfYear(underlying)

  def hours = Hours(underlying)

  def minutes = Minutes(underlying)

  def seconds = Seconds(underlying)

  def toISO8601 = ToISO8601(underlying)

  def toEpochTime = ToEpochTime(underlying)


  //def inTimeZone(timezone:DateTimeZone)= inTimeZone(timezone.)

}