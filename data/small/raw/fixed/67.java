@java.lang.Override
public void handleError(org.forest.http.ForestRequest request, org.forest.http.ForestResponse response) {
    org.forest.exceptions.ForestNetworkException networkException = new org.forest.exceptions.ForestNetworkException("", response.getStatusCode(), response);
    handleError(request, response, networkException);
}