package org.ceylon.idea.lang.parser.parsing;

import com.intellij.lang.PsiBuilder;
import com.intellij.psi.JavaTokenType;
import org.ceylon.idea.lang.lexer.CeylonTokenType;
import org.ceylon.idea.lang.parser.ParserUtils;

import static org.ceylon.idea.lang.parser.CeylonElementTypes.IMPORT;

public class ImportStatement {
    public static boolean parse(PsiBuilder builder) {
        PsiBuilder.Marker marker = builder.mark();

        if (!ParserUtils.getToken(builder, CeylonTokenType.IMPORT_KEYWORD)) {
            marker.rollbackTo();
            return false;
        }

        ParserUtils.getToken(builder, CeylonTokenType.LIDENTIFIER, "import.name.expected");
        ParserUtils.getToken(builder, JavaTokenType.LBRACE, "import.lbrace.expected");
        ParserUtils.getToken(builder, JavaTokenType.ELLIPSIS, "import.ellipsis.expected");
        ParserUtils.getToken(builder, JavaTokenType.RBRACE, "import.rbrace.expected");

        marker.done(IMPORT);
        return true;
    }

}
