private int getIndex(java.lang.String key) {
    return (key.hashCode()) % (table.length);
}