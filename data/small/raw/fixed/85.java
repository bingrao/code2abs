@java.lang.Override
public void terminate() {
    currentTransaction.markForTermination(Status.Transaction.Terminated);
}