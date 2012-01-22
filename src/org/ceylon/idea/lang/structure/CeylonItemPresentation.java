package org.ceylon.idea.lang.structure;

import com.intellij.navigation.ItemPresentation;
import com.intellij.openapi.util.Iconable;
import com.intellij.psi.PsiElement;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

public class CeylonItemPresentation implements ItemPresentation {
    private final PsiElement myElement;

    public CeylonItemPresentation(PsiElement myElement) {
        this.myElement = myElement;
    }

    @Override
    public String getPresentableText() {
        return myElement.toString();
    }

    @Nullable
    public String getLocationString() {
        return null;
    }

    @Nullable
    public Icon getIcon(boolean open) {
        return myElement.getIcon(open ? Iconable.ICON_FLAG_OPEN : Iconable.ICON_FLAG_OPEN);
    }
}
