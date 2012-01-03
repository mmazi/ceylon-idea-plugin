package org.ceylon.idea.lang.psi;

import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.intellij.lang.ASTNode;

public class CeylonLIdentifier extends ASTWrapperPsiElement {
    public CeylonLIdentifier(ASTNode node) {
        super(node);
    }
}
