@java.lang.Override
public void onClick(android.view.View v) {
    if (net.anei.cadpage.SmsPopupUtils.haveNet(this)) {
        this.finish();
    }
}