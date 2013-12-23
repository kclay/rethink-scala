package com.rethinkscala.reflect

import com.fasterxml.jackson.module.scala.{JacksonModule, DefaultScalaModule}
import com.fasterxml.jackson.datatype.joda.JodaModule
import com.fasterxml.jackson.databind.deser.Deserializers
import com.fasterxml.jackson.datatype.joda.deser._
import org.joda.time._
import java.lang.Object
import com.fasterxml.jackson.databind.{BeanDescription, DeserializationConfig, JavaType}
import com.fasterxml.jackson.databind.module.SimpleDeserializers

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 12/18/13
 * Time: 6:09 PM 
 */
class RethinkModule extends DefaultScalaModule {

  private val _deserializers = new SimpleDeserializers()
  _deserializers.addDeserializer(classOf[DateTime], RethinkDateTimeDeserializer.forType(classOf[DateTime]))
  _deserializers.addDeserializer(classOf[ReadableDateTime], RethinkDateTimeDeserializer.forType(classOf[ReadableDateTime]))
  _deserializers.addDeserializer(classOf[ReadableInstant], RethinkDateTimeDeserializer.forType(classOf[ReadableInstant]))


  //dd//
  // this += { _.addDeserializers(Date) }
  this += (_ addDeserializers _deserializers)

  override def getModuleName = "RethinkModule"
}


object RethinkModule extends RethinkModule with JodaModule

