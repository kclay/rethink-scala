package com.rethinkscala.net

import com.rethinkscala.reflect.Reflector
import com.typesafe.scalalogging.slf4j.LazyLogging
import org.jboss.netty.buffer.ChannelBuffer
import org.jboss.netty.channel.{Channel, ChannelHandlerContext}
import org.jboss.netty.handler.codec.frame.FrameDecoder

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 8/16/14
 * Time: 11:30 AM
 *
 */


abstract class RethinkFrameDecoder(readAmount: Int) extends FrameDecoder {

  def decode(buffer: ChannelBuffer): AnyRef

  def decode(ctx: ChannelHandlerContext, channel: Channel, buffer: ChannelBuffer): AnyRef = {
    buffer.markReaderIndex()


    if (!buffer.readable() || buffer.readableBytes() < readAmount) {
      buffer.resetReaderIndex()
      return null
    }
    decode(buffer)
  }
}

class JsonFrameDecoder extends RethinkFrameDecoder(12) with LazyLogging {
  override def decode(buffer: ChannelBuffer): AnyRef = {
    val token = buffer.readLong()
    val length = buffer.readInt()

    if (buffer.readableBytes() < length) {
      buffer.resetReaderIndex()
      return null
    }

    val json = new String(buffer.readBytes(length).array(), "US-ASCII")
    logger.debug(s"Decoding Frame TOKEN = $token LENGTH = $length ")
    logger.debug(s"JSON = $json");
    (token, json)

  }
}

class ProtoFrameDecoder extends RethinkFrameDecoder(4) {
  override def decode(buffer: ChannelBuffer): AnyRef = {
    val length = buffer.readInt()

    if (buffer.readableBytes() < length) {
      buffer.resetReaderIndex()
      return null
    }
    buffer.readBytes(length)
  }
}

