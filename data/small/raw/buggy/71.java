public void onClick(android.content.DialogInterface dialog, int id) {
    ru.velkonost.lume.activity.BoardCardActivity.LeaveCard leaveCard = new ru.velkonost.lume.activity.BoardCardActivity.LeaveCard();
    leaveCard.execute();
    refreshActivity();
}