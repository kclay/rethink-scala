package com.rethinkscala.reflect

import com.fasterxml.jackson.databind.{SerializerProvider, JsonNode, SerializationFeature}
import org.joda.time.DateTime
import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.ser.std.StdSerializer
import com.fasterxml.jackson.databind.jsontype.TypeSerializer
import java.lang.reflect.Type

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 12/23/13
 * Time: 2:57 PM 
 */
/*
class RethinkDateTimeSerializer extends StdSerializer[DateTime](classOf[DateTime]) {
  def serialize(value: DateTime, jgen: JsonGenerator, provider: SerializerProvider) {
    if (provider.isEnabled(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS)) {
      jgen.writeNumber(value.getMillis)
    }
    else {
      jgen.writeString(value.toString)
    }
  }

  override def serializeWithType(value: DateTime, jgen: JsonGenerator, provider: SerializerProvider, typeSer: TypeSerializer) {
    typeSer.writeTypePrefixForScalar(value, jgen)
    serialize(value, jgen, provider)
    typeSer.writeTypeSuffixForScalar(value, jgen)
  }

  override def getSchema(provider: SerializerProvider, typeHint: Type): JsonNode = {
    createSchemaNode(if (provider.isEnabled(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS)) "number" else "string", true)
  }
}
    */