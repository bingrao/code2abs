@java.lang.Override
public void handleError(org.forest.http.ForestRequest request, org.forest.http.ForestResponse response) {
    org.forest.exceptions.ForestNetworkException networkException = new org.forest.exceptions.ForestNetworkException("", response.getStatusCode(), response);
    org.forest.exceptions.ForestRuntimeException e = new org.forest.exceptions.ForestRuntimeException(networkException);
    handleError(request, response, e);
}