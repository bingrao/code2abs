public void cancelPieceIndexRequest(java.lang.Integer peerid) {
    lock.writeLock().lock();
    this.idxBeingRequested.remove(peerid);
    lock.writeLock().unlock();
}