@java.lang.Override
public void onConnectionFailed(@android.support.annotation.NonNull
com.google.android.gms.common.ConnectionResult result) {
    if ((mSignInProgress) != (com.texocoyotl.ptd2googlelogin.MainActivity.STATE_IN_PROGRESS)) {
        mSignInIntent = result.getResolution();
        if ((mSignInProgress) == (com.texocoyotl.ptd2googlelogin.MainActivity.STATE_SIGNING_IN)) {
            resolveSignInError();
        }
    }
    onSignedOut();
}