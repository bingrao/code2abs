public com.stefanmuenchow.arithmetic.Arithmetic<X> neg() {
    value = getOperations().neg(value);
    return this;
}