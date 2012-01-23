package org.ceylon.idea.lang.structure;

import com.intellij.ide.structureView.StructureViewTreeElement;
import com.intellij.ide.structureView.TextEditorBasedStructureViewModel;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.util.PsiTreeUtil;
import org.ceylon.idea.lang.psi.CeylonPsi;
import org.jetbrains.annotations.NotNull;

public class CeylonStructureViewModel extends TextEditorBasedStructureViewModel {

    public CeylonStructureViewModel(@NotNull PsiFile psiFile) {
        super(psiFile);
    }

    @NotNull
    @Override
    public StructureViewTreeElement getRoot() {
        PsiElement compilationUnit = PsiTreeUtil.getChildOfType(getPsiFile(), CeylonPsi.CompilationUnit.class);
        return new CeylonStructureViewElement(compilationUnit);
    }

}
