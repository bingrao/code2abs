@java.lang.Override
public <L extends java.util.List<at.reisisoft.Tokenizer.j8.lexerrules.expressions.JavaSimpleToken> & java.util.RandomAccess> boolean isApplicable(L javaSimpleTokens, int fromPos) {
    return !(JavaSimpleTokenType.SCOPESTART.equals(javaSimpleTokens.get(fromPos).getTokenType()));
}