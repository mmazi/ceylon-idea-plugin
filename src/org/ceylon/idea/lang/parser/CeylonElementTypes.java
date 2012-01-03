package org.ceylon.idea.lang.parser;

import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.IFileElementType;
import org.ceylon.idea.lang.lexer.CeylonElementType;

public interface CeylonElementTypes {
    IFileElementType FILE = new CeylonFileElementType();
    IElementType NLS = new CeylonElementType("NLS");
    IElementType IMPORT = new CeylonElementType("IMPORT");
    IElementType IMPORT_LIST = new CeylonElementType("IMPORT_LIST");
    IElementType TOP_LEVEL_DECLARATION_LIST = new CeylonElementType("TOP_LEVEL_DECLARATION_LIST");
    IElementType METHOD_DECLARATION = new CeylonElementType("METHOD_DECLARATION");
    IElementType L_IDENTIFIER = new CeylonElementType("L_L_IDENTIFIER");
}
