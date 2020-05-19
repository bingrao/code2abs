@java.lang.Override
public void onPeerDisconnected(com.mobvoi.android.wearable.Node node) {
    android.util.Log.d(com.theteamgo.fancywatch.MainActivity.TAG, ("Node Disconnected" + (node.getId())));
}