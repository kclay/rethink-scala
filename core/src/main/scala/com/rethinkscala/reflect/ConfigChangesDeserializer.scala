package com.rethinkscala.reflect

import com.fasterxml.jackson.core.{JsonToken, JsonParser}
import com.fasterxml.jackson.databind.DeserializationContext
import com.fasterxml.jackson.databind.deser.std.StdScalarDeserializer
import com.rethinkscala.{TableConfigResults, ConfigChanges}

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 3/7/15
 * Time: 6:43 PM
 *
 */
object ConfigChangesDeserializer extends StdScalarDeserializer[ConfigChanges](classOf[ConfigChanges]){
  type Val = Option[TableConfigResults]
  val typeRefVal = Reflector.typeReference[Option[TableConfigResults]]

  override def deserialize(jp: JsonParser, ctxt: DeserializationContext) = {
    jp.nextToken() // skip array
    var newVal:Val = None
    var oldVal:Val = None
    jp.nextToken()

    var token:JsonToken =null
    try {
      for (i <- 1 to 2) {
        jp.getCurrentName match {
          case "new_val" =>
            token = jp.nextToken();
            newVal = jp.readValueAs(typeRefVal)

          case "old_val" =>
            token = jp.nextToken();
            oldVal = jp.readValueAs(typeRefVal)
        }

      token = jp.nextToken()

      }

    }catch {
      case e:Exception=> {
        val a = e
        println(e)
      }
    }
    token = jp.nextToken()
    var v = jp.getCurrentName


    ConfigChanges(oldVal,newVal)
  }
}
