package com.rethinkscala.net

import java.nio.ByteOrder
import java.util

import com.typesafe.scalalogging.slf4j.LazyLogging
import io.netty.buffer.ByteBuf
import io.netty.channel.ChannelHandlerContext
import io.netty.handler.codec.ByteToMessageDecoder

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 8/16/14
 * Time: 11:30 AM
 *
 */


abstract class RethinkFrameDecoder(readAmount: Int) extends ByteToMessageDecoder {

  override def decode(ctx: ChannelHandlerContext, buffer: ByteBuf, out: util.List[AnyRef]) = {
    buffer.markReaderIndex()
    if (!buffer.isReadable || buffer.readableBytes() < readAmount) {
      buffer.resetReaderIndex()
    } else {
      decode(out, buffer.order(ByteOrder.LITTLE_ENDIAN))
    }

  }

  def decode(out: util.List[AnyRef], buffer: ByteBuf): Unit


}

class JsonFrameDecoder extends RethinkFrameDecoder(12) with LazyLogging {
  override def decode(out: util.List[AnyRef], buffer: ByteBuf): Unit = {

    val token = buffer.readLong()
    val length = buffer.readInt()

    if (buffer.readableBytes() < length) {
      buffer.resetReaderIndex()

    } else {

      val json = new String(buffer.readBytes(length).array(), "UTF-8")
      logger.debug(s"Decoding Frame TOKEN = $token LENGTH = $length ")
      if (json.length > 500) {
        logger.debug(s"JSON = ${json.substring(0, 499)}")
      } else {
        logger.debug(s"JSON = $json")
      }

      out.add((token, json))
    }

  }
}

class ProtoFrameDecoder extends RethinkFrameDecoder(4) {
  override def decode(out: util.List[AnyRef], buffer: ByteBuf): Unit = {
    val length = buffer.readInt()

    if (buffer.readableBytes() < length) {
      buffer.resetReaderIndex()

    } else {
      out.add(buffer.readBytes(length))
    }
  }
}

