package org.ceylon.idea.lang.parser;

import com.intellij.lang.ASTNode;
import com.intellij.lang.PsiBuilder;
import com.intellij.lang.PsiParser;
import com.intellij.psi.tree.IElementType;
import org.jetbrains.annotations.NotNull;


public class CeylonParser implements PsiParser {
    @NotNull
    @Override
    public ASTNode parse(IElementType root, PsiBuilder builder) {
        builder.setDebugMode(true);

        PsiBuilder.Marker rootMarker = builder.mark();

//        new ParseHelper(builder).parseCompilationUnit();
        Grammar.CompilationUnit.parseRequired(builder);

        while (!builder.eof()) {
            builder.advanceLexer();
        }

        rootMarker.done(root);

        return builder.getTreeBuilt();
    }

}
