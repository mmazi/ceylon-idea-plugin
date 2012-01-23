package org.ceylon.idea.lang.psi;

import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.intellij.lang.ASTNode;
import org.jetbrains.annotations.NotNull;

public class CeylonPsi {

    public static class Method extends ASTWrapperPsiElement {
        public Method(@NotNull ASTNode node) {
            super(node);
        }
    }

    public static class CompilationUnit extends ASTWrapperPsiElement {
        public CompilationUnit(@NotNull ASTNode node) {
            super(node);
        }
    }
}
