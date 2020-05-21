private boolean handleHolds(model.patron.Patron patron) {
    this.printHoldAlertMessage(patron);
    return this.dealWithEachHold(patron);
}