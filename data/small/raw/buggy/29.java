public void Send(java.lang.String data, int rindex) {
    data = data.trim().replaceAll("\r?\n", "");
    ServerHandler.Send(data, rindex);
}