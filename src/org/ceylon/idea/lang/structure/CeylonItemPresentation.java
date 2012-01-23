package org.ceylon.idea.lang.structure;

import com.intellij.navigation.ItemPresentation;
import com.intellij.openapi.util.Iconable;
import com.intellij.psi.PsiElement;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

public class CeylonItemPresentation implements ItemPresentation {
    private final PsiElement psiElement;

    public CeylonItemPresentation(PsiElement psiElement) {
        this.psiElement = psiElement;
    }

    @Override
    public String getPresentableText() {
        return psiElement.toString();
    }

    @Nullable
    public String getLocationString() {
        return null;
    }

    @Nullable
    public Icon getIcon(boolean open) {
        return psiElement.getIcon(open ? Iconable.ICON_FLAG_OPEN : Iconable.ICON_FLAG_OPEN);
    }
}
