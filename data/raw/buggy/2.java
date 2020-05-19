public com.google.common.util.concurrent.ListenableFuture<com.microsoft.windowsazure.mobileservices.authentication.MobileServiceUser> login(com.microsoft.windowsazure.mobileservices.authentication.MobileServiceAuthenticationProvider provider, java.lang.String oAuthToken) {
    return login(provider.toString(), oAuthToken);
}