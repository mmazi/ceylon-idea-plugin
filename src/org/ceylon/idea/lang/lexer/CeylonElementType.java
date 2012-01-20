package org.ceylon.idea.lang.lexer;

import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.TokenSet;
import org.ceylon.idea.Ceylon;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

import static org.ceylon.idea.lang.lexer.CeylonToken.*;

public class CeylonElementType extends IElementType {

    public static final TokenSet KEYWORD_SET = TokenSet.create(ABSTRACTED_TYPE.getElementType(), ADAPTED_TYPES.getElementType(),
            ASSIGN.getElementType(), BREAK.getElementType(), CASE_CLAUSE.getElementType(), CASE_TYPES.getElementType(),
            CATCH_CLAUSE.getElementType(), CLASS_DEFINITION.getElementType(), CONTINUE.getElementType(),
            ELSE_CLAUSE.getElementType(), EXISTS.getElementType(), EXTENDS.getElementType(), FINALLY_CLAUSE.getElementType(),
            FOR_CLAUSE.getElementType(), FUNCTION_MODIFIER.getElementType(), IF_CLAUSE.getElementType(), IMPORT.getElementType(),
            INTERFACE_DEFINITION.getElementType(), NONEMPTY.getElementType(), OBJECT_DEFINITION.getElementType(),
            OUT.getElementType(), OUTER.getElementType(), RETURN.getElementType(), SATISFIES.getElementType(),
            SUPER.getElementType(), SWITCH_CLAUSE.getElementType(), THEN_CLAUSE.getElementType(), THIS.getElementType(),
            THROW.getElementType(), TRY_CLAUSE.getElementType(), TYPE_CONSTRAINT.getElementType(),
            VALUE_MODIFIER.getElementType(), VOID_MODIFIER.getElementType(), WHILE_CLAUSE.getElementType());

    public static final TokenSet WHITE_SPACE_SET = TokenSet.create(WS.getElementType());
    public static final TokenSet COMMENT_SET = TokenSet.create(MULTI_COMMENT.getElementType());
    public static final TokenSet STRING_LITERAL_SET = TokenSet.create(STRING_LITERAL.getElementType());


    private final int type;


    public CeylonElementType(@NotNull @NonNls CeylonToken token) {
        super(token.name(), Ceylon.LANGUAGE);
        this.type = token.getType();
    }

    public int getType() {
        return type;
    }
}
