@java.lang.Override
public void onMessage(final org.phoenixframework.channels.Envelope envelope) {
    java.lang.String reason = null;
    if (envelope != null) {
        reason = envelope.getReason();
    }
    callback.onError(reason);
}