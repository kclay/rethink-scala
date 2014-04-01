package com.rethinkscala.reflect

import com.fasterxml.jackson.core.{JsonToken, JsonParser}
import com.fasterxml.jackson.databind.{JsonDeserializer, DeserializationContext}
import com.fasterxml.jackson.core.JsonToken._

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 3/26/2014
 * Time: 4:17 PM 
 */
trait ReqlTypeDeserializer[T] {

  val KEY_REQL_TYPE = "$reql_type$"

  val reqlTypeValue:String

  val dataStartKey:String




  def deserialize(jp: JsonParser, ctxt: DeserializationContext): T = {

    implicit val p = jp
    implicit val ctx = ctxt
    if (jp.getCurrentToken eq JsonToken.START_OBJECT) {


      return resolveType(next).asInstanceOf[T]
    }


    throw ctxt.mappingException("Error")
  }

  def resolveType(token: JsonToken)(implicit ctx: DeserializationContext, jp: JsonParser): Any = {
    token match {

      case FIELD_NAME => jp.getCurrentName match {
        case x:String if(x == dataStartKey) =>resolve(next)
        case x:String if(x == KEY_REQL_TYPE) => {
          next
          if (jp.getValueAsString != reqlTypeValue) throw ctx.mappingException(s"Invalid $KEY_REQL_TYPE of ${jp.getValueAsString}")
          resolveType(next)
        }
      }
      case _=> throw ctx.mappingException(s"Expected token FIELD_NAME")
    }
  }

  def resolve(token: JsonToken)(implicit ctx: DeserializationContext, jp: JsonParser): Any

  def next(implicit jp: JsonParser) = jp.nextToken()



}
