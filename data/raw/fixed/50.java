private static byte setBit(byte b, int pos) {
    b |= ((byte) (1 << pos));
    return b;
}