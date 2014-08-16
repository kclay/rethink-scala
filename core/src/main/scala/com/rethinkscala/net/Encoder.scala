package com.rethinkscala.net

import java.nio.ByteOrder

import org.jboss.netty.buffer.ChannelBuffers._
import org.jboss.netty.channel.{Channel, ChannelHandlerContext}
import org.jboss.netty.handler.codec.oneone.OneToOneEncoder
import ql2.Ql2
import ql2.Ql2.VersionDummy
import ql2.{Ql2 => ql2}
import ql2.Response.ResponseType
import ql2.{Response, VersionDummy}

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 8/16/14
 * Time: 11:50 AM
 *
 */

class RethinkDBEncoder extends OneToOneEncoder {

  def encode(ctx: ChannelHandlerContext, channel: Channel, msg: Any): AnyRef = {

    msg match {
      case v: VersionDummy.Version => {
        val b = buffer(ByteOrder.LITTLE_ENDIAN, 4)
        b.writeInt(v.getNumber)
        b
      }
      case p:ql2.VersionDummy.Protocol=>{
        val b = buffer(ByteOrder.LITTLE_ENDIAN,4)
        b.writeInt(p.getNumber)
        b
      }
      case q: CompiledQuery => q.encode
      case s: String =>
        val b = buffer(ByteOrder.LITTLE_ENDIAN, s.length + 4)
        b.writeInt(s.length)
        b.writeBytes(s.getBytes("ascii"))
        b
      case q: ql2.Query =>
        val size = q.getSerializedSize
        val b = buffer(ByteOrder.LITTLE_ENDIAN, size + 4)
        b.writeInt(size)

        b.writeBytes(q.toByteArray)
        b
    }

  }
}

