package org.ceylon.idea.lang.lexer;

import com.intellij.psi.tree.IElementType;
import org.ceylon.idea.Ceylon;
import org.jetbrains.annotations.NotNull;

public class CeylonElementType extends IElementType {
    public CeylonElementType(@NotNull String debugName) {
        super(debugName, Ceylon.LANGUAGE);
    }
}
