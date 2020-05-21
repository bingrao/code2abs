@java.lang.Override
public void follower() {
    undoRegiser();
    if (((server) != null) && (server.isRunning())) {
        server.stopAndWait();
    }
}