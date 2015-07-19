package com.rethinkscala.backend.netty

import java.net.InetSocketAddress
import java.util.concurrent.atomic.AtomicLong

import com.rethinkscala.net.Version
import com.rethinkscala.utils.ConnectionFactory
import com.typesafe.scalalogging.slf4j.LazyLogging
import io.netty.bootstrap.Bootstrap

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 7/17/2015
 * Time: 6:09 PM 
 */
case class NettyConnectionFactory(version: Version, bootstrap: Bootstrap, connectionId: AtomicLong) extends ConnectionFactory[ConnectionChannel] with LazyLogging {

  def create(): ConnectionChannel = {

    logger.debug("Creating new ChannelWrapper")
    val c = bootstrap.connect(new InetSocketAddress(version.host, version.port)).sync()

    val cf = ChannelAttribute.Future.get(c.channel())

    new ConnectionChannel(cf, connectionId.incrementAndGet())
  }

  def configure(wrapper: ConnectionChannel) = {
    wrapper.active.set(true)
    if (!wrapper.configured) {
      logger.debug("Configuring ChannelWrapper")
      //  version.configure(wrapper.channel)
      wrapper.configured = true
    } else {
      logger.debug("Logger already configured")
    }
  }

  def validate(wrapper: ConnectionChannel): Boolean = {
    wrapper.channel.isOpen
  }

  def destroy(wrapper: ConnectionChannel) {
    logger.debug("Destroying Channel")
    wrapper.invalidated = true
    wrapper.channel.close()
  }


}
