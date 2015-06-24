package com.rethinkscala.reflect

import java.util

import com.fasterxml.jackson.annotation.JsonTypeInfo
import com.fasterxml.jackson.databind.ObjectMapper.DefaultTypeResolverBuilder
import com.fasterxml.jackson.databind.`type`.CollectionLikeType
import com.fasterxml.jackson.databind.jsontype.impl.StdTypeResolverBuilder
import com.fasterxml.jackson.databind.jsontype.{NamedType, TypeDeserializer}
import com.fasterxml.jackson.databind.{ObjectMapper, DeserializationConfig, JavaType}
import com.rethinkscala._

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 3/29/14
 * Time: 6:43 PM
 *
 */

class RethinkTypeResolverBuilder(t: ObjectMapper.DefaultTyping) extends DefaultTypeResolverBuilder(t) {





}
