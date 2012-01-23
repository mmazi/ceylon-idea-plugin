package org.ceylon.idea.lang.structure;

import com.intellij.ide.structureView.StructureViewTreeElement;
import com.intellij.ide.util.treeView.smartTree.TreeElement;
import com.intellij.navigation.ItemPresentation;
import com.intellij.pom.Navigatable;
import com.intellij.psi.PsiElement;

public class CeylonStructureViewElement implements StructureViewTreeElement {
    final protected PsiElement psiElement;

    protected CeylonStructureViewElement(PsiElement psiElement) {
        this.psiElement = psiElement;
    }

    public Object getValue() {
        return psiElement.isValid() ? psiElement : null;
    }

    public void navigate(boolean b) {
        ((Navigatable) psiElement).navigate(b);
    }

    public boolean canNavigate() {
        return ((Navigatable) psiElement).canNavigate();
    }

    public boolean canNavigateToSource() {
        return ((Navigatable) psiElement).canNavigateToSource();
    }

    @Override
    public ItemPresentation getPresentation() {
        return new CeylonItemPresentation(psiElement);
    }

    @Override
    public TreeElement[] getChildren() {
        PsiElement[] psiChildren = psiElement.getChildren();
        TreeElement[] result = new TreeElement[psiChildren.length];
        for (int i = 0; i < psiChildren.length; i++) {
            result[i] = new CeylonStructureViewElement(psiChildren[i]);
        }
        return result;
    }
}
