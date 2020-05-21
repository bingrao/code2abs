@java.lang.Override
public int howManyStartsWithPrefix(java.lang.String prefix) {
    if ((prefix.length()) == 0) {
        return 0;
    }
    return node.howManyStartsWithPrefix(prefix, 0);
}