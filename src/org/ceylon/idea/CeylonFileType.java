package org.ceylon.idea;

import com.intellij.openapi.fileTypes.LanguageFileType;
import com.intellij.openapi.util.IconLoader;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

public class CeylonFileType extends LanguageFileType {

    CeylonFileType() {
        super(Ceylon.LANGUAGE);
    }

    @NotNull
    @Override
    public String getName() {
        return "Ceylon";
    }

    @NotNull
    @Override
    public String getDescription() {
        return "Ceylon files";
    }

    @NotNull
    @Override
    public String getDefaultExtension() {
        return "ceylon";
    }

    @Override
    public Icon getIcon() {
        return IconLoader.getIcon("ceylon.png");
    }
}
