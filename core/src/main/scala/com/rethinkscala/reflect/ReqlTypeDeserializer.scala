package com.rethinkscala.reflect

import com.fasterxml.jackson.core.{JsonToken, JsonParser}
import com.fasterxml.jackson.databind.{JsonMappingException, DeserializationContext}
import org.joda.time.{DateTime, DateTimeZone, ReadableDateTime}
import com.fasterxml.jackson.core.JsonToken._
import scala.collection.mutable.ArrayBuffer

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 3/26/2014
 * Time: 4:17 PM 
 */
trait ReqlTypeDeserializer[T] {

  private[this] val KEY_REQL_TYPE = "$reql_type$"

  type Matcher = PartialFunction[(String, JsonParser), Unit]
  val matchers: ArrayBuffer[Matcher] = ArrayBuffer.empty[Matcher]

  def step(m: Matcher): Unit = matchers += m

  def constructType: T

  def deserialize(jp: JsonParser, ctxt: DeserializationContext): T = {
    var t = jp.getCurrentToken
    if (t eq JsonToken.START_OBJECT) {
      t = jp.nextToken
      var fieldType: String = null

      var bundle: (String, JsonParser) = null
      do {
        if (t eq VALUE_STRING) {
          if (jp.getCurrentName eq KEY_REQL_TYPE) {
            fieldType = jp.getValueAsString
          }
          else {
            bundle = (jp.getCurrentName, jp)
            matchers.map(f => if (f.isDefinedAt(bundle)) f.apply(bundle))
          }

        }


        t = jp.nextToken
      } while (t ne JsonToken.END_OBJECT)

      return constructType
    }


    throw mappingException(ctxt)
  }

  def mappingException(ctxt: DeserializationContext): JsonMappingException

}
