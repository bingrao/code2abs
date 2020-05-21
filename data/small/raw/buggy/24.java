@java.lang.Override
public void onResume() {
    android.util.Log.d(org.eyeseetea.malariacare.fragments.MonitorFragment.TAG, "onResume");
    setListShown(false);
    registerSurveysReceiver();
    super.onResume();
}