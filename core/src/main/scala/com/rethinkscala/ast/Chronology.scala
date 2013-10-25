package com.rethinkscala.ast

import org.joda.time.DateTime
import ql2.Ql2.Term.TermType
import com.rethinkscala.BoundOptions

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 10/1/13
 * Time: 12:55 AM
 *
 */


/*
case class Time(value:DateTime) extends ProduceTime{

  override lazy val args = Seq(value.year(),value.monthOfYear(),value.dayOfMonth(),
  value.hourOfDay(),value.minuteOfHour(),value.secondOfMinute(),value.dat)

  def termType = TermType.TIME
} */

abstract class TimeExtractor(tt:TermType) extends ProduceNumeric{
  val value:TimeTyped

  override lazy val args = buildArgs(value)

  def termType =  tt
}


case class ISO8601(value:String) extends ProduceTime{
  def termType = TermType.ISO8601
}

case class ToISO8601(value:TimeTyped) extends ProduceString{
  def termType = TermType.TO_ISO8601
}

case class EpochTime(value:Long) extends ProduceTime{
  def termType = TermType.EPOCH_TIME
}
case class ToEpochTime(value:TimeTyped) extends ProduceNumeric{
  def termType = TermType.TO_EPOCH_TIME
}

class Now extends ProduceTime{
  def termType = TermType.NOW
}

case class InTimeZone(value:TimeTyped,zone:Either[TimeTyped,String]) extends ProduceTime{

  override lazy val args = buildArgs(value,zone.fold(identity,identity))

  def termType = TermType.IN_TIMEZONE
}


case class During(value:TimeTyped,start:TimeTyped,end:TimeTyped,bounds:Option[BoundOptions]=None) extends ProduceBinary{

  override lazy val args = buildArgs(value,start,end)
  override lazy val optargs = buildOptArgs(bounds.map(_.toMap).getOrElse(Map()))

  def termType = TermType.DURING
}

case class Date(value:TimeTyped) extends ProduceTime{
  def termType = TermType.DATE
}
case class TimeOfDay(value:TimeTyped) extends ProduceNumeric{
  def termType = TermType.TIME_OF_DAY
}
case class Timezone(value:TimeTyped) extends ProduceString{
  def termType = TermType.TIMEZONE
}
case class Year(value:TimeTyped) extends TimeExtractor(TermType.YEAR)


case class Month(value:TimeTyped)  extends TimeExtractor(TermType.MONTH)

case class Day(value:TimeTyped) extends TimeExtractor(TermType.DAY)
case class DayOfWeek(value:TimeTyped) extends TimeExtractor(TermType.DAY_OF_WEEK)
case class DayOfYear(value:TimeTyped) extends TimeExtractor(TermType.DAY_OF_YEAR)
case class Hours(value:TimeTyped) extends TimeExtractor(TermType.HOURS)
case class Minutes(value:TimeTyped) extends TimeExtractor(TermType.MINUTES)

case class Seconds(value:TimeTyped) extends TimeExtractor(TermType.SECONDS)

case class TimeName(tt:TermType) extends ProduceNumeric{

  override protected val extractArgs = false

  def termType = tt
}



