package com.rethinkscala.net

import java.util.concurrent.{TimeUnit, LinkedTransferQueue}

import io.netty.channel.{SimpleChannelInboundHandler, ChannelHandlerContext, ChannelInboundHandlerAdapter, ChannelInboundHandler}

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 4/7/2015
 * Time: 11:56 AM 
 */
trait BlockingException extends Exception

class BlockingReadTimeoutException extends BlockingException


class BlockingChannelClosedException extends BlockingException

class BlockingReadHandler[T] extends ChannelInboundHandlerAdapter {

  val queue = new LinkedTransferQueue[Any]()
  @volatile
  var closed = false

  def read(timeout: Long, unit: TimeUnit): T = {
    val e = readEvent(timeout, unit)
    if (e == null) {
      return null.asInstanceOf[T]
    }
    e
  }


  def isClosed = closed

  @throws[InterruptedException]
  @throws[BlockingReadTimeoutException]
  def readEvent(timeout: Long, unit: TimeUnit): T = {
    detectDeadLock()
    if (isClosed) {
      if (queue.isEmpty) {
        return null.asInstanceOf[T]
      }
    }

    val e = queue.poll(timeout, unit)
    if (e == null) {
      throw new BlockingReadTimeoutException()
    } else {
      e.asInstanceOf[T]
    }
  }


  override def handlerRemoved(ctx: ChannelHandlerContext) = {
    super.handlerRemoved(ctx)
    queue.clear()
  }

  def detectDeadLock() = false

  override def channelRead(ctx: ChannelHandlerContext, msg: scala.Any) = {
    queue.put(msg)
  }


  override def channelInactive(ctx: ChannelHandlerContext) = {
    super.channelInactive(ctx)
    queue.put(new BlockingChannelClosedException)
    closed = true
  }

  override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable) = {
    queue.put(cause)
  }
}
