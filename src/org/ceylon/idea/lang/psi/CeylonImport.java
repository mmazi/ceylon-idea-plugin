package org.ceylon.idea.lang.psi;

import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.intellij.lang.ASTNode;

public class CeylonImport extends ASTWrapperPsiElement {
    public CeylonImport(ASTNode node) {
        super(node);
    }
}
