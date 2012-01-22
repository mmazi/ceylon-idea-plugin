package org.ceylon.idea.lang.structure;

import com.intellij.ide.structureView.StructureViewTreeElement;
import com.intellij.ide.structureView.TextEditorBasedStructureViewModel;
import com.intellij.psi.PsiFile;
import org.jetbrains.annotations.NotNull;

public class CeylonStructureViewModel extends TextEditorBasedStructureViewModel {

    protected CeylonStructureViewModel(@NotNull PsiFile psiFile) {
        super(psiFile);
    }

    @NotNull
    @Override
    public StructureViewTreeElement getRoot() {
        return new CeylonStructureViewElement(getPsiFile());
    }
}
