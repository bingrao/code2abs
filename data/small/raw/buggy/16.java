public static io.netty.channel.ChannelFuture asyncWriteToEntity(final com.zx.sms.connect.manager.EndpointEntity entity, final java.lang.Object msg) {
    com.zx.sms.connect.manager.EndpointConnector connector = EndpointManager.INS.getEndpointConnector(entity);
    return com.zx.sms.common.util.ChannelUtil.asyncWriteToEntity(connector, msg);
}