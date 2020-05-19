private void startServer() {
    server = new server.Server(this.logger);
    server.start();
}