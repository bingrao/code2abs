class JavaApp {
    int a = 3;
    public boolean hasViewOrContainer(java.lang.String ref) {
        return (getViewCreatorIndex(ref)) >= 0;
    }
}