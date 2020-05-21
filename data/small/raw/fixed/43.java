@java.lang.Override
public void follower() {
    if (((server) != null) && (server.isRunning())) {
        server.stopAndWait();
    }
    undoRegiser();
}