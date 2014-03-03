package com.rethinkscala.reflect

import com.fasterxml.jackson.datatype.joda.deser.DateTimeDeserializer
import org.joda.time.{ReadableDateTime, DateTime, DateTimeZone, ReadableInstant}
import com.fasterxml.jackson.databind.JsonDeserializer
import java.lang.String
import com.fasterxml.jackson.core._
import com.fasterxml.jackson.databind.DeserializationContext
import com.fasterxml.jackson.databind.jsontype.TypeDeserializer
import com.fasterxml.jackson.core.JsonToken._

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 12/23/13
 * Time: 11:09 AM 
 */


object RethinkDateTimeDeserializer {
  def forType[T <: ReadableInstant](cls: Class[T]): JsonDeserializer[T] = {
    new RethinkDateTimeDeserializer(cls).asInstanceOf[JsonDeserializer[T]]
  }
}

class RethinkDateTimeDeserializer(cls: Class[_ <: ReadableInstant]) extends DateTimeDeserializer(cls) {

  private[this] val KEY_REQL_TYPE = "$reql_type$"
  private[this] val KEY_TIMEZONE = "timezone"

  override def deserializeWithType(jp: JsonParser, ctxt: DeserializationContext, typeDeserializer: TypeDeserializer): AnyRef = {
    typeDeserializer.deserializeTypedFromAny(jp, ctxt)
  }

  override def deserialize(jp: JsonParser, ctxt: DeserializationContext): ReadableDateTime = {
    var t = jp.getCurrentToken
    if (t eq JsonToken.START_OBJECT) {
      t = jp.nextToken
      var fieldType: String = null
      var epochTime: Long = -1
      var timeZone: DateTimeZone = DateTimeZone.getDefault
      do {
        if (t eq VALUE_STRING) {
          if (jp.getCurrentName eq KEY_REQL_TYPE) {
            fieldType = jp.getValueAsString
          }
          else if (jp.getCurrentName eq KEY_TIMEZONE) {
            timeZone = DateTimeZone.forID(jp.getValueAsString)
          }
        }
        else if (t eq VALUE_NUMBER_FLOAT) {
          epochTime = jp.getLongValue * 1000
        }
        t = jp.nextToken
      } while (t ne JsonToken.END_OBJECT)


      if (epochTime != -1 && fieldType != null && (fieldType == "TIME")) {
        return new DateTime(epochTime, timeZone)
      }
    }



    throw ctxt.mappingException(getValueClass)
  }
}

