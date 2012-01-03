package org.ceylon.idea.lang.psi;

import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.intellij.lang.ASTNode;

public class CeylonImportList extends ASTWrapperPsiElement {

    public CeylonImportList(ASTNode node) {
        super(node);
    }
}
