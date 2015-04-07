package com.rethinkscala.net

import java.nio.ByteOrder

import io.netty.buffer.ByteBuf
import io.netty.channel.{ChannelHandler, ChannelHandlerContext}
import io.netty.handler.codec.MessageToByteEncoder
import ql2.{Ql2 => ql2}


/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 8/16/14
 * Time: 11:50 AM
 *
 */


@ChannelHandler.Sharable
class RethinkDBEncoder extends MessageToByteEncoder[Any] {


  override def encode(ctx: ChannelHandlerContext, msg: Any, buffer: ByteBuf) = {

    val out = buffer.order(ByteOrder.LITTLE_ENDIAN)

    msg match {
      case v: ql2.VersionDummy.Version =>

        out.capacity(4).writeInt(v.getNumber)

      case p: ql2.VersionDummy.Protocol =>
        out.capacity(4).writeInt(p.getNumber)

      case q: CompiledQuery => {
        q.encode(out)
      }
      case s: String =>
        out.capacity(s.length + 4)
          .writeInt(s.length)
          .writeBytes(s.getBytes("ascii"))

      case q: ql2.Query =>
        val size = q.getSerializedSize
        out.capacity(size + 4)
          .writeInt(size)
          .writeBytes(q.toByteArray)

    }
  }


}

