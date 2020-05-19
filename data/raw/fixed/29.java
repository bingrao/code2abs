public void Send(java.lang.String data, int rindex) {
    data = data.trim().replaceAll("\r?\n", "");
    if (data.equals(""))
        return ;
    
    ServerHandler.Send(data, rindex);
}