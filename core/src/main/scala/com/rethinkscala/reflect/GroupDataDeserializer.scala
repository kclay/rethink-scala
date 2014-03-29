package com.rethinkscala.reflect

import com.rethinkscala.GroupResult
import com.fasterxml.jackson.databind.deser.std.StdScalarDeserializer
import com.fasterxml.jackson.databind.{BeanProperty, DeserializationContext, JsonDeserializer}
import com.fasterxml.jackson.databind.deser.ContextualDeserializer
import com.fasterxml.jackson.core.JsonParser
import com.fasterxml.jackson.core.JsonToken._

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 3/26/2014
 * Time: 4:12 PM 
 */
class GroupDataDeserializer extends JsonDeserializer[GroupResult[_]] with ContextualDeserializer with ReqlTypeDeserializer[GroupResult[_]] {
  private var targetClass: Class[_] = _


  private[this] val KEY_REQL_TYPE = "$reql_type$"
  private[this] val KEY_TIMEZONE = "GROUPED_DATA"


  step({
    case ("data", jp) => if (jp.nextToken() == START_ARRAY) {
      jp.nextToken() // START_ARRAY

    }
  })

  override def createContextual(ctxt: DeserializationContext, property: BeanProperty) = {
    val javaType = property.getType()
    val ofType = javaType.containedType(0)
    targetClass = ofType.getRawClass()
    this
  }

  override def constructType = ???

  override def mappingException(ctxt: DeserializationContext) = ctxt.mappingException(targetClass)
}
