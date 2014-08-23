package com.rethinkscala.net

import org.jboss.netty.handler.codec.oneone.OneToOneDecoder
import org.jboss.netty.buffer.ChannelBuffer
import com.google.protobuf.{ExtensionRegistry, MessageLite}
import org.jboss.netty.channel.{Channel, ChannelHandlerContext, ChannelHandler}

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 6/1/14
 * Time: 11:29 AM
 *
 */



object ProtobufDecoder{

  def apply(prototype: ql2.Ql2.Response) = new ProtobufDecoder(prototype.getDefaultInstanceForType,null)
}
@ChannelHandler.Sharable
case class ProtobufDecoder( prototype: ql2.Ql2.Response, extensionRegistry: ExtensionRegistry) extends OneToOneDecoder {

  private final val HAS_PARSER: Boolean = false


  protected def decode(ctx: ChannelHandlerContext, channel: Channel, msg: AnyRef): AnyRef = {
    if (!(msg.isInstanceOf[ChannelBuffer])) {
      return msg
    }
    val buf: ChannelBuffer = msg.asInstanceOf[ChannelBuffer]
    var  array: Array[Byte] = null
    var offset: Int = 0
    val length: Int = buf.readableBytes
    if (buf.hasArray) {
      array = buf.array
      offset = buf.arrayOffset + buf.readerIndex
    }
    else {
      array = new Array[Byte](length)
      buf.getBytes(buf.readerIndex, array, 0, length)
      offset = 0
    }
    var response:ql2.Ql2.Response = null
    if (extensionRegistry == null) {
      response =  prototype.newBuilderForType.mergeFrom(array, offset, length).build
    } else {
      response =  prototype.newBuilderForType.mergeFrom(array, offset, length, extensionRegistry).build
    }
    response
  }


}
