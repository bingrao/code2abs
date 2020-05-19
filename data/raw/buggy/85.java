@java.lang.Override
public void terminate() {
    currentTransaction.get().markForTermination(Status.Transaction.Terminated);
}