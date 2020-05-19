public synchronized void setCommitIndex(int commitIndex) {
    if ((this.commitIndex) <= commitIndex) {
        this.commitIndex = commitIndex;
    }else {
        return ;
    }
}