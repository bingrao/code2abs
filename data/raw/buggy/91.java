@java.lang.Override
public byte getColumn(int x) {
    return patterns[us.cownet.lamps.GreyscaleLampPattern.INDEX[cycleCount]].getColumn(x);
}