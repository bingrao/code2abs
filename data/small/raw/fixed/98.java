private int getIndex(java.lang.String key) {
    return ((key.hashCode()) >>> 1) % (table.length);
}