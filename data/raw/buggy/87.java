@java.lang.Override
public void onMessage(final org.phoenixframework.channels.Envelope envelope) {
    final java.lang.String reason = envelope.getReason();
    callback.onError(reason);
}