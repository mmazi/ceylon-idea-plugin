package org.ceylon.idea;

import com.intellij.lexer.Lexer;
import com.intellij.openapi.editor.HighlighterColors;
import com.intellij.openapi.editor.SyntaxHighlighterColors;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.fileTypes.SyntaxHighlighterBase;
import com.intellij.psi.tree.IElementType;
import org.ceylon.idea.lang.lexer.CeylonElementType;
import org.ceylon.idea.lang.lexer.CeylonLexerAdapter;
import org.ceylon.idea.lang.lexer.CeylonToken;
import org.jetbrains.annotations.NotNull;

import java.util.HashMap;
import java.util.Map;

public class CeylonSyntaxHighlighter extends SyntaxHighlighterBase {
    private static final Map<IElementType, TextAttributesKey> attributes = new HashMap<IElementType, TextAttributesKey>();

    static {
        attributes.put(CeylonToken.MULTI_COMMENT.getElementType(), SyntaxHighlighterColors.JAVA_BLOCK_COMMENT);
        attributes.put(CeylonToken.BAD_CHARACTER.getElementType(), HighlighterColors.BAD_CHARACTER);
        fillMap(attributes, CeylonElementType.STRING_LITERAL_SET, SyntaxHighlighterColors.STRING);
        fillMap(attributes, CeylonElementType.KEYWORD_SET, SyntaxHighlighterColors.KEYWORD);
    }

    @NotNull
    @Override
    public Lexer getHighlightingLexer() {
        return new CeylonLexerAdapter();
    }

    @NotNull
    @Override
    public TextAttributesKey[] getTokenHighlights(IElementType tokenType) {
        return pack(attributes.get(tokenType));
    }
}
