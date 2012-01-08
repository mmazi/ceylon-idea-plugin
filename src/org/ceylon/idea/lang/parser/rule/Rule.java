package org.ceylon.idea.lang.parser.rule;

import com.intellij.lang.PsiBuilder;

@SuppressWarnings("unused")
public interface Rule {

    boolean parseRequired(PsiBuilder builder);

    boolean parseOptional(PsiBuilder builder);


}
