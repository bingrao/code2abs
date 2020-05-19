private void handleHolds(model.patron.Patron patron) {
    this.printHoldAlertMessage(patron);
    this.dealWithEachHold(patron);
}