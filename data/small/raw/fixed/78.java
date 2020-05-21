@org.jetbrains.annotations.Nullable
@java.lang.Override
public com.intellij.psi.PsiElement findElementForParameterInfo(@org.jetbrains.annotations.NotNull
com.intellij.lang.parameterInfo.CreateParameterInfoContext context) {
    int offset = context.getOffset();
    return context.getFile().findElementAt(offset).getParent().getParent();
}