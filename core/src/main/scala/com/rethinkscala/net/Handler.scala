package com.rethinkscala.net

import org.jboss.netty.channel.{ChannelHandlerContext, ExceptionEvent, MessageEvent, SimpleChannelUpstreamHandler}
import ql2.Ql2.Response

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 8/16/14
 * Time: 11:45 AM
 *
 */


class RethinkChannelHandler[T] extends SimpleChannelUpstreamHandler {
  implicit def channelHandlerContext2Promise(ctx: ChannelHandlerContext): Option[Token[T]] = Some(ctx.getChannel.getAttachment.asInstanceOf[Token[T]])

  override def messageReceived(ctx: ChannelHandlerContext, e: MessageEvent) {
    ctx.map(_.handle(e.getMessage.asInstanceOf[T]))
  }

  override def exceptionCaught(ctx: ChannelHandlerContext, e: ExceptionEvent) {
    ctx.map(_.failure(e.getCause))
  }
}

class ProtoChannelHandler extends RethinkChannelHandler[Response]

class JsonChannelHandler extends RethinkChannelHandler[String]

