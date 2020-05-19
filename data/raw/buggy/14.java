public static java.math.BigInteger generatePositiveRand(int numBits) {
    return new java.math.BigInteger(numBits, new java.security.SecureRandom());
}