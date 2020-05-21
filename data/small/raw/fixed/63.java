@java.lang.Override
public void onStart() {
    super.onStart();
    android.util.Log.i(com.shimastripe.gpsmountainview.MainActivity.TAG, "onStart");
    android.util.Log.i(com.shimastripe.gpsmountainview.MainActivity.TAG, "Connect to Google Api");
    mGoogleApiClient.connect();
}