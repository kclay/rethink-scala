package com.rethinkscala.reflect

import com.fasterxml.jackson.databind.module.{SimpleAbstractTypeResolver, SimpleDeserializers}
import com.fasterxml.jackson.module.scala._
import com.rethinkscala._
import org.joda.time._

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
  _deserializers.addDeserializer(classOf[GroupResult[_]], new GroupResultDeserializer)
  _deserializers.addDeserializer(classOf[JsonDocument], new JsonDocumentDeserializer)
  _deserializers.addDeserializer(classOf[Polygon], PolygonDeserializer)
  _deserializers.addDeserializer(classOf[Point], PointDeserializer)
  _deserializers.addDeserializer(classOf[UnknownGeometry], UnknownGeometryDeserializer)
  _deserializers.addDeserializer(classOf[Line], LineDeserializer)
  _deserializers.addDeserializer(classOf[PolygonSubResults], PolygonSubResultsDeserializer)
  _deserializers.addDeserializer(classOf[ConfigChanges], ConfigChangesDeserializer)
  _deserializers.addDeserializer(classOf[Durability.Kind],DurabilityDeserializer)


  private val _resolver = new SimpleAbstractTypeResolver
  _resolver.addMapping(classOf[Document], classOf[BasicDocument])
  //_resolver.addMapping(classOf[Seq[GroupResultRecord[_]]],)


  //dd//
  // this += { _.addDeserializers(Date) }
  this += (_ addDeserializers _deserializers)
  this += (_ addAbstractTypeResolver _resolver)

  override def getModuleName = "RethinkModule"
}


object RethinkModule extends RethinkModule



