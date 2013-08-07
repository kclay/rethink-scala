package com.rethinkscala.net;

import com.google.protobuf.ExtensionRegistry;
import com.google.protobuf.MessageLite;
import org.jboss.netty.buffer.ChannelBuffer;
import org.jboss.netty.channel.Channel;
import org.jboss.netty.channel.ChannelHandler;
import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.handler.codec.oneone.OneToOneDecoder;

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 8/6/13
 * Time: 8:02 PM
 */
@ChannelHandler.Sharable
public class ProtobufDecoder2 extends OneToOneDecoder {
    private static final boolean HAS_PARSER = false;  // fix for ScalaBuff 1.3.3
    /*
     static {
         boolean hasParser = false;
         try {
             // MessageLite.getParsetForType() is not available until protobuf 2.5.0.
             MessageLite.class.getDeclaredMethod("getParseprForType");
             hasParser = true;
         } catch (Throwable t) {
             // Ignore
         }

         HAS_PARSER = hasParser;
     }  */
    private final MessageLite prototype;
    private final ExtensionRegistry extensionRegistry;

    /**
     * Creates a new instance.
     */
    public ProtobufDecoder2(MessageLite prototype) {
        this(prototype, null);
    }

    public ProtobufDecoder2(MessageLite prototype, ExtensionRegistry extensionRegistry) {
        if (prototype == null) {
            throw new NullPointerException("prototype");
        }
        this.prototype = prototype.getDefaultInstanceForType();
        this.extensionRegistry = extensionRegistry;
    }

    @Override
    protected Object decode(
            ChannelHandlerContext ctx, Channel channel, Object msg) throws Exception {
        if (!(msg instanceof ChannelBuffer)) {
            return msg;
        }

        ChannelBuffer buf = (ChannelBuffer) msg;
        final byte[] array;
        final int offset;
        final int length = buf.readableBytes();

        if (buf.hasArray()) {
            array = buf.array();
            offset = buf.arrayOffset() + buf.readerIndex();
        } else {
            array = new byte[length];
            buf.getBytes(buf.readerIndex(), array, 0, length);
            offset = 0;
        }

        if (extensionRegistry == null) {

            return prototype.newBuilderForType().mergeFrom(array, offset, length).build();

        } else {

            return prototype.newBuilderForType().mergeFrom(array, offset, length, extensionRegistry).build();

        }
    }
}