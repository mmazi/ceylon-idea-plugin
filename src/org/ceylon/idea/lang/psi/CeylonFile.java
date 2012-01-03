package org.ceylon.idea.lang.psi;

import com.intellij.extapi.psi.PsiFileBase;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.psi.FileViewProvider;
import org.ceylon.idea.Ceylon;
import org.jetbrains.annotations.NotNull;

public class CeylonFile extends PsiFileBase {

    public CeylonFile(@NotNull FileViewProvider viewProvider) {
        super(viewProvider, Ceylon.LANGUAGE);
    }

    @NotNull
    @Override
    public FileType getFileType() {
        return Ceylon.FILE_TYPE;
    }
}
