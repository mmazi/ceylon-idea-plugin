package org.ceylon.idea.lang.structure;

import com.intellij.ide.structureView.StructureViewTreeElement;
import com.intellij.ide.util.treeView.smartTree.TreeElement;
import com.intellij.navigation.ItemPresentation;
import com.intellij.pom.Navigatable;
import com.intellij.psi.PsiElement;

public class CeylonStructureViewElement implements StructureViewTreeElement {
    final protected PsiElement myElement;

    protected CeylonStructureViewElement(PsiElement myElement) {
        this.myElement = myElement;
    }

    public Object getValue() {
        return myElement.isValid() ? myElement : null;
    }

    public void navigate(boolean b) {
        ((Navigatable) myElement).navigate(b);
    }

    public boolean canNavigate() {
        return ((Navigatable) myElement).canNavigate();
    }

    public boolean canNavigateToSource() {
        return ((Navigatable) myElement).canNavigateToSource();
    }

    @Override
    public ItemPresentation getPresentation() {
        return new CeylonItemPresentation(myElement);
    }

    @Override
    public TreeElement[] getChildren() {
        return new TreeElement[0];
    }
}
