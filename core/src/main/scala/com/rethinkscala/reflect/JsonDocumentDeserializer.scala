package com.rethinkscala.reflect

import com.fasterxml.jackson.core.JsonParser
import com.fasterxml.jackson.databind.jsontype.TypeDeserializer
import com.fasterxml.jackson.databind.node.{BaseJsonNode, ObjectNode, ArrayNode}
import com.fasterxml.jackson.databind.{DeserializationContext, JsonDeserializer}
import com.rethinkscala.JsonDocument


/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 8/23/14
 * Time: 1:42 PM
 *
 */
class JsonDocumentDeserializer extends JsonDeserializer[JsonDocument] {


  override def deserialize(jp: JsonParser, ctx: DeserializationContext) = {

    val node = Reflector.mapper.readTree[BaseJsonNode](jp)


    new JsonDocument(node.toString)
  }

}
