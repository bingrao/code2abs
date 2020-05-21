@java.lang.Override
public void gotResult(int responseCode, java.lang.String responseMsg, long groupId) {
    if (responseCode == 0) {
        callback.success(java.lang.String.valueOf(groupId));
    }else {
        callback.error(responseMsg);
    }
}