@java.lang.Override
protected void onCreate(android.os.Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    setContentView(R.layout.activity_main);
    com.architecture.realarchitecture.domain.request.Request<java.util.Map<java.lang.String, java.lang.Object>> request = new com.architecture.realarchitecture.presention.request.DemoObjectRequest("testObject");
    enqueueRequest(request);
}