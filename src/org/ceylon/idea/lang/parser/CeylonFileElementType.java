package org.ceylon.idea.lang.parser;

import com.intellij.psi.tree.ILightStubFileElementType;
import org.ceylon.idea.Ceylon;

public class CeylonFileElementType extends ILightStubFileElementType {
    public CeylonFileElementType() {
        super(Ceylon.LANGUAGE);
    }
}
