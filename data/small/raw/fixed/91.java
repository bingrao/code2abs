@java.lang.Override
public byte getColumn(int x) {
    return patterns[index[cycleCount]].getColumn(x);
}