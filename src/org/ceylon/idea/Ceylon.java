package org.ceylon.idea;

import com.intellij.lang.Language;
import com.intellij.openapi.fileTypes.FileType;

public final class Ceylon {
    public static final Language LANGUAGE = new CeylonLanguage();
    public static final FileType FILE_TYPE = new CeylonFileType();


    private Ceylon() {
    }


}
