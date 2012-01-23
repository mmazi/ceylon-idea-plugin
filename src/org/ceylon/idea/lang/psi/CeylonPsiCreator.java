package org.ceylon.idea.lang.psi;

import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import org.ceylon.idea.lang.parser.CeylonElementTypes;

public class CeylonPsiCreator {

    /**
     * Creates Ceylon PSI element by given AST node
     *
     * @param node Given node
     * @return Respective PSI element
     */
    public static PsiElement createElement(ASTNode node) {
        if (node.getElementType() == CeylonElementTypes.CompilationUnit) return new CeylonPsi.CompilationUnit(node);
        if (node.getElementType() == CeylonElementTypes.Method) return new CeylonPsi.Method(node);
        return new ASTWrapperPsiElement(node);
    }

}
