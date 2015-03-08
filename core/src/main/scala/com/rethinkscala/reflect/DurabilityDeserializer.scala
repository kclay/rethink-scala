package com.rethinkscala.reflect

import com.fasterxml.jackson.core.JsonParser
import com.fasterxml.jackson.databind.DeserializationContext
import com.fasterxml.jackson.databind.deser.std.StdScalarDeserializer
import com.rethinkscala.Durability

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 3/7/15
 * Time: 7:31 PM
 *
 */
object DurabilityDeserializer extends StdScalarDeserializer[Durability.Kind](classOf[Durability.Kind]){

  override def deserialize(jp: JsonParser, ctxt: DeserializationContext) = {
    val value = jp.getValueAsString
    if(value == Durability.Hard.toString) Durability.Hard else Durability.Soft
  }
}
