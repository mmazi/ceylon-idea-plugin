package org.ceylon.idea.lang.structure;

import com.intellij.ide.structureView.StructureViewBuilder;
import com.intellij.ide.structureView.StructureViewModel;
import com.intellij.ide.structureView.TreeBasedStructureViewBuilder;
import com.intellij.lang.PsiStructureViewFactory;
import com.intellij.psi.PsiFile;
import org.jetbrains.annotations.NotNull;

public class CeylonStructureViewFactory implements PsiStructureViewFactory {
    public StructureViewBuilder getStructureViewBuilder(final PsiFile psiFile) {
        return new TreeBasedStructureViewBuilder() {

            @NotNull
            public StructureViewModel createStructureViewModel() {
                return new CeylonStructureViewModel(psiFile);
            }
        };
    }
}