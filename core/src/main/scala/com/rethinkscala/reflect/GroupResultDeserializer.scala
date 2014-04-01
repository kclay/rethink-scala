package com.rethinkscala.reflect

import com.fasterxml.jackson.databind._
import com.fasterxml.jackson.core.JsonParser
import com.rethinkscala.GroupResult
import com.fasterxml.jackson.databind.jsontype.TypeDeserializer

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 3/26/2014
 * Time: 4:12 PM 
 */
class GroupResultDeserializer extends JsonDeserializer[GroupResult[_]] {

  override def deserializeWithType(jp: JsonParser, ctxt: DeserializationContext, typeDeserializer: TypeDeserializer) = super.deserializeWithType(jp, ctxt, typeDeserializer)

  override def deserialize(jp: JsonParser, ctxt: DeserializationContext) = throw ctxt.mappingException(classOf[GroupResult[_]])
}
