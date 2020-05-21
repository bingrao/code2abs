private void startServer() {
    server = new server.Server(this.logger);
    futureTask = new java.util.concurrent.FutureTask<java.lang.Void>(server, null);
    futureTask.run();
}