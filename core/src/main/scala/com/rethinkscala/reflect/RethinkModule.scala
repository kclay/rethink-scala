package com.rethinkscala.reflect

import com.fasterxml.jackson.module.scala._
import org.joda.time._
import com.fasterxml.jackson.databind.module.{SimpleAbstractTypeResolver, SimpleDeserializers}
import com.rethinkscala._
import com.fasterxml.jackson.module.scala.deser.UntypedObjectDeserializerModule

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
  _deserializers.addDeserializer(classOf[GroupResult[_]],new GroupResultDeserializer)
  _deserializers.addDeserializer(classOf[JsonDocument],new JsonDocumentDeserializer)






  private val _resolver = new SimpleAbstractTypeResolver
  _resolver.addMapping(classOf[Document],classOf[BasicDocument])
  //_resolver.addMapping(classOf[Seq[GroupResultRecord[_]]],)


  //dd//
  // this += { _.addDeserializers(Date) }
  this += (_ addDeserializers _deserializers)
  this += ( _ addAbstractTypeResolver _resolver)

  override def getModuleName = "RethinkModule"
}


object RethinkModule extends RethinkModule



