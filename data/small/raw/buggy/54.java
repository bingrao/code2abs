public synchronized void setCommitIndex(int commitIndex) {
    assert (this.commitIndex) <= commitIndex;
    this.commitIndex = commitIndex;
}