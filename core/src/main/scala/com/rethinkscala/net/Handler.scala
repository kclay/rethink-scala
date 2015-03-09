package com.rethinkscala.net

import com.typesafe.scalalogging.LazyLogging
import org.jboss.netty.channel.{ChannelHandlerContext, ExceptionEvent, MessageEvent, SimpleChannelUpstreamHandler}
import org.jboss.netty.handler.queue.BufferedWriteHandler
import ql2.Ql2.Response

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 8/16/14
 * Time: 11:45 AM
 *
 */


class RethinkChannelHandler[T] extends SimpleChannelUpstreamHandler {
  type Handler = ConnectionAttachment[T]

  implicit def channelHandlerContext2Promise(ctx: ChannelHandlerContext) = Some(ctx.getChannel.getAttachment.asInstanceOf[Handler])

  override def messageReceived(ctx: ChannelHandlerContext, e: MessageEvent) {
    val (tokenId, response) = e.getMessage.asInstanceOf[(Long, T)]
    ctx.map(_.handle(tokenId, response))
  }

  override def exceptionCaught(ctx: ChannelHandlerContext, e: ExceptionEvent) {
    ctx.map(_.failure(e.getCause))
  }
}

class ProtoChannelHandler extends RethinkChannelHandler[Response]

class JsonChannelHandler extends RethinkChannelHandler[String]


