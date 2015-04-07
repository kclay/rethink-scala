package com.rethinkscala.net

import io.netty.channel.{ChannelHandler, ChannelHandlerContext, Channel, ChannelInboundHandlerAdapter}
import io.netty.util.AttributeKey
import ql2.Ql2.Response

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 8/16/14
 * Time: 11:45 AM
 *
 */


object ChannelAttribute {
  private[this] val _handlerAttr = AttributeKey.valueOf[Any]("Rethink.handler")

  object Handler {
    def get[T](channel: Channel) = channel.attr(handlerAttr[T]).get()

    def set[T](channel: Channel, attachment: ConnectionAttachment[T]) = {
      channel.attr(handlerAttr(attachment)).set(attachment)
    }
  }

  def handlerAttr[T] = _handlerAttr.asInstanceOf[AttributeKey[ConnectionAttachment[T]]]

  def handlerAttr[T](value: T) = _handlerAttr.asInstanceOf[AttributeKey[T]]
}

@ChannelHandler.Sharable
class RethinkChannelHandler[T] extends ChannelInboundHandlerAdapter {

  import ChannelAttribute.Handler

  implicit def channelHandlerContext2Promise(ctx: ChannelHandlerContext) = Some(Handler.get[T](ctx.channel()))


  override def channelRead(ctx: ChannelHandlerContext, msg: scala.Any) = {
    val (tokenId, response) = msg.asInstanceOf[(Long, T)]
    ctx.map(_.handle(tokenId, response))
  }


  override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable) = {
    ctx.map(_.failure(cause))
  }


}

class ProtoChannelHandler extends RethinkChannelHandler[Response]

class JsonChannelHandler extends RethinkChannelHandler[String]


