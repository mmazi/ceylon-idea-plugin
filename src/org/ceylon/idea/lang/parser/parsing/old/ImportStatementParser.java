package org.ceylon.idea.lang.parser.parsing.old;

import com.intellij.lang.PsiBuilder;
import org.ceylon.idea.lang.lexer.CeylonToken;
import org.ceylon.idea.lang.parser.CeylonAstNode;
import org.ceylon.idea.lang.parser.ParserUtils;

public class ImportStatementParser {
    public static boolean parse(PsiBuilder builder) {
        PsiBuilder.Marker marker = builder.mark();

        if (!ParserUtils.getToken(builder, CeylonToken.IMPORT)) {
            marker.rollbackTo();
            return false;
        }

        ParserUtils.getToken(builder, CeylonToken.LIDENTIFIER, "import.name.expected");
        ParserUtils.getToken(builder, CeylonToken.LBRACE, "import.lbrace.expected");
        ParserUtils.getToken(builder, CeylonToken.ELLIPSIS, "import.ellipsis.expected");
        ParserUtils.getToken(builder, CeylonToken.RBRACE, "import.rbrace.expected");

        marker.done(CeylonAstNode.IMPORT);
        return true;
    }

}
