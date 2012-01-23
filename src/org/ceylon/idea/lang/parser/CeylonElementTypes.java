package org.ceylon.idea.lang.parser;

import com.intellij.psi.tree.IElementType;
import org.ceylon.idea.Ceylon;

public class CeylonElementTypes {

    public static final IElementType CompilationUnit = new IElementType("CompilationUnit", Ceylon.LANGUAGE);
    public static final IElementType Declaration = new IElementType("Declaration", Ceylon.LANGUAGE);
    public static final IElementType Method = new IElementType("Method", Ceylon.LANGUAGE);
}
