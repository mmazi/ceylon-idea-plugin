package org.ceylon.idea.lang.parser.parsing;

import com.intellij.lang.PsiBuilder;

public class CompilationUnit {

    public static void parse(PsiBuilder builder) {
        ImportList.parse(builder);
    }
}
