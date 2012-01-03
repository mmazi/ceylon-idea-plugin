package org.ceylon.idea.lang.parser.parsing;

import com.intellij.lang.PsiBuilder;
import org.ceylon.idea.lang.lexer.CeylonTokenType;
import org.ceylon.idea.lang.parser.ParserUtils;

import static org.ceylon.idea.lang.parser.CeylonElementTypes.IMPORT_LIST;

public class ImportList {

    public static void parse(PsiBuilder builder) {
        PsiBuilder.Marker marker = builder.mark();

        while (ParserUtils.lookAhead(builder, CeylonTokenType.IMPORT_KEYWORD)) {
            ImportStatement.parse(builder);
            ParserUtils.skipNLS(builder);
        }

        marker.done(IMPORT_LIST);
    }

}
