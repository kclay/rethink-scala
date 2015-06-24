package com.rethinkscala.reflect

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.SerializerProvider
import com.fasterxml.jackson.databind.ser.std.StdSerializer
import com.rethinkscala.ast.Expr
import org.joda.time.{ReadableInstant, DateTime}
import org.joda.time.format.ISODateTimeFormat


/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 12/23/13
 * Time: 2:57 PM 
 */

object RethinkDateTimeSerializer extends StdSerializer[ReadableInstant](classOf[ReadableInstant]) {
  override def serialize(date: ReadableInstant, jgen: JsonGenerator, provider: SerializerProvider) = {
    val value = Expr(date)
    jgen.writeString(value.serialize)
  }
}

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