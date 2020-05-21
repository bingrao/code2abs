public void startGame(java.lang.String filename) {
    this.thread = new net.ccmob.apps.jpushy.core.LevelThread(filename);
    java.lang.Thread t = new java.lang.Thread(thread);
    t.start();
}