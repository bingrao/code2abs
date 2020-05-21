public void ReleaseButton(int i) {
    SendData(releaseKey[i]);
    android.util.Log.i("iBeatCon", (("Button " + i) + " Released"));
}