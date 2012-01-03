package org.ceylon.idea;

import com.intellij.lexer.Lexer;
import com.intellij.openapi.editor.HighlighterColors;
import com.intellij.openapi.editor.SyntaxHighlighterColors;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.fileTypes.SyntaxHighlighterBase;
import com.intellij.psi.tree.IElementType;
import org.ceylon.idea.lang.lexer.CeylonLexer;
import org.ceylon.idea.lang.lexer.CeylonTokenType;
import org.jetbrains.annotations.NotNull;

import java.util.HashMap;
import java.util.Map;

public class CeylonSyntaxHighlighter extends SyntaxHighlighterBase {
    private static final Map<IElementType, TextAttributesKey> attributes = new HashMap<IElementType, TextAttributesKey>();

    static {
        attributes.put(CeylonTokenType.C_STYLE_COMMENT, SyntaxHighlighterColors.JAVA_BLOCK_COMMENT);
        attributes.put(CeylonTokenType.STRING_LITERAL, SyntaxHighlighterColors.STRING);
        attributes.put(CeylonTokenType.BAD_CHARACTER, HighlighterColors.BAD_CHARACTER);
        fillMap(attributes, CeylonTokenType.KEYWORD_SET, SyntaxHighlighterColors.KEYWORD);
    }

    @NotNull
    @Override
    public Lexer getHighlightingLexer() {
        return new CeylonLexer();
    }

    @NotNull
    @Override
    public TextAttributesKey[] getTokenHighlights(IElementType tokenType) {
        return pack(attributes.get(tokenType));
    }
}
