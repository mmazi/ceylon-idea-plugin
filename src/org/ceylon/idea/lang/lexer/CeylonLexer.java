package org.ceylon.idea.lang.lexer;

import com.intellij.lexer.FlexAdapter;

import java.io.Reader;

public class CeylonLexer extends FlexAdapter {

    public CeylonLexer() {
        super(new _CeylonLexer((Reader) null));
    }
}
