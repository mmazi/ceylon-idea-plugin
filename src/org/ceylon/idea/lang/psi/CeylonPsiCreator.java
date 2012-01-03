package org.ceylon.idea.lang.psi;

import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;

public class CeylonPsiCreator {

    /**
     * Creates Ceylon PSI element by given AST node
     *
     * @param node Given node
     * @return Respective PSI element
     */
    public static PsiElement createElement(ASTNode node) {
        return new ASTWrapperPsiElement(node);
    }

}
