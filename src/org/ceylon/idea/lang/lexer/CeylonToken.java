package org.ceylon.idea.lang.lexer;

import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.TokenSet;
import org.ceylon.idea.Ceylon;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

public class CeylonToken extends IElementType {
    public static final CeylonToken BAD_CHARACTER = new CeylonToken("BAD_CHARACTER");

    public static final CeylonToken ABSTRACTED_TYPE = new CeylonToken("ABSTRACTED_TYPE");
    public static final CeylonToken ADAPTED_TYPES = new CeylonToken("ADAPTED_TYPES");
    public static final CeylonToken ADD_ASSIGN_OP = new CeylonToken("ADD_ASSIGN_OP");
    public static final CeylonToken AND_ASSIGN_OP = new CeylonToken("AND_ASSIGN_OP");
    public static final CeylonToken AND_OP = new CeylonToken("AND_OP");
    public static final CeylonToken APPLY_OP = new CeylonToken("APPLY_OP");
    public static final CeylonToken ARRAY = new CeylonToken("ARRAY");
    public static final CeylonToken ASSIGN = new CeylonToken("ASSIGN");
    public static final CeylonToken ASSIGN_OP = new CeylonToken("ASSIGN_OP");
    public static final CeylonToken BREAK = new CeylonToken("BREAK");
    public static final CeylonToken CASE_CLAUSE = new CeylonToken("CASE_CLAUSE");
    public static final CeylonToken CASE_TYPES = new CeylonToken("CASE_TYPES");
    public static final CeylonToken CATCH_CLAUSE = new CeylonToken("CATCH_CLAUSE");
    public static final CeylonToken CHAR_LITERAL = new CeylonToken("CHAR_LITERAL");
    public static final CeylonToken CLASS_DEFINITION = new CeylonToken("CLASS_DEFINITION");
    public static final CeylonToken COMMA = new CeylonToken("COMMA");
    public static final CeylonToken COMPARE_OP = new CeylonToken("COMPARE_OP");
    public static final CeylonToken COMPILER_ANNOTATION = new CeylonToken("COMPILER_ANNOTATION");
    public static final CeylonToken COMPLEMENT_ASSIGN_OP = new CeylonToken("COMPLEMENT_ASSIGN_OP");
    public static final CeylonToken COMPLEMENT_OP = new CeylonToken("COMPLEMENT_OP");
    public static final CeylonToken CONTINUE = new CeylonToken("CONTINUE");
    public static final CeylonToken DECREMENT_OP = new CeylonToken("DECREMENT_OP");
    public static final CeylonToken DEFAULT_OP = new CeylonToken("DEFAULT_OP");
    public static final CeylonToken DIFFERENCE_OP = new CeylonToken("DIFFERENCE_OP");
    public static final CeylonToken Digit = new CeylonToken("Digit");
    public static final CeylonToken Digits = new CeylonToken("Digits");
    public static final CeylonToken DIVIDE_ASSIGN_OP = new CeylonToken("DIVIDE_ASSIGN_OP");
    public static final CeylonToken ELLIPSIS = new CeylonToken("ELLIPSIS");
    public static final CeylonToken ELSE_CLAUSE = new CeylonToken("ELSE_CLAUSE");
    public static final CeylonToken ENTRY_OP = new CeylonToken("ENTRY_OP");
    public static final CeylonToken EQUAL_OP = new CeylonToken("EQUAL_OP");
    public static final CeylonToken EscapeSequence = new CeylonToken("EscapeSequence");
    public static final CeylonToken EXISTS = new CeylonToken("EXISTS");
    public static final CeylonToken Exponent = new CeylonToken("Exponent");
    public static final CeylonToken EXTENDS = new CeylonToken("EXTENDS");
    public static final CeylonToken FINALLY_CLAUSE = new CeylonToken("FINALLY_CLAUSE");
    public static final CeylonToken FLOAT_LITERAL = new CeylonToken("FLOAT_LITERAL");
    public static final CeylonToken FOR_CLAUSE = new CeylonToken("FOR_CLAUSE");
    public static final CeylonToken FractionalMagnitude = new CeylonToken("FractionalMagnitude");
    public static final CeylonToken FUNCTION_MODIFIER = new CeylonToken("FUNCTION_MODIFIER");
    public static final CeylonToken IDENTICAL_OP = new CeylonToken("IDENTICAL_OP");
    public static final CeylonToken IdentifierPart = new CeylonToken("IdentifierPart");
    public static final CeylonToken IdentifierStart = new CeylonToken("IdentifierStart");
    public static final CeylonToken IF_CLAUSE = new CeylonToken("IF_CLAUSE");
    public static final CeylonToken IMPORT = new CeylonToken("IMPORT");
    public static final CeylonToken INCREMENT_OP = new CeylonToken("INCREMENT_OP");
    public static final CeylonToken INDEX_OP = new CeylonToken("INDEX_OP");
    public static final CeylonToken IN_OP = new CeylonToken("IN_OP");
    public static final CeylonToken INTERFACE_DEFINITION = new CeylonToken("INTERFACE_DEFINITION");
    public static final CeylonToken INTERSECT_ASSIGN_OP = new CeylonToken("INTERSECT_ASSIGN_OP");
    public static final CeylonToken INTERSECTION_OP = new CeylonToken("INTERSECTION_OP");
    public static final CeylonToken IS_OP = new CeylonToken("IS_OP");
    public static final CeylonToken LARGE_AS_OP = new CeylonToken("LARGE_AS_OP");
    public static final CeylonToken LARGER_OP = new CeylonToken("LARGER_OP");
    public static final CeylonToken LBRACE = new CeylonToken("LBRACE");
    public static final CeylonToken LBRACKET = new CeylonToken("LBRACKET");
    public static final CeylonToken Letter = new CeylonToken("Letter");
    public static final CeylonToken LIDENTIFIER = new CeylonToken("LIDENTIFIER");
    public static final CeylonToken LINE_COMMENT = new CeylonToken("LINE_COMMENT");
    public static final CeylonToken LPAREN = new CeylonToken("LPAREN");
    public static final CeylonToken Magnitude = new CeylonToken("Magnitude");
    public static final CeylonToken MEMBER_OP = new CeylonToken("MEMBER_OP");
    public static final CeylonToken MULTI_COMMENT = new CeylonToken("MULTI_COMMENT");
    public static final CeylonToken MULTIPLY_ASSIGN_OP = new CeylonToken("MULTIPLY_ASSIGN_OP");
    public static final CeylonToken NATURAL_LITERAL = new CeylonToken("NATURAL_LITERAL");
    public static final CeylonToken NonCharacterChars = new CeylonToken("NonCharacterChars");
    public static final CeylonToken NONEMPTY = new CeylonToken("NONEMPTY");
    public static final CeylonToken NonStringChars = new CeylonToken("NonStringChars");
    public static final CeylonToken NOT_EQUAL_OP = new CeylonToken("NOT_EQUAL_OP");
    public static final CeylonToken NOT_OP = new CeylonToken("NOT_OP");
    public static final CeylonToken OBJECT_DEFINITION = new CeylonToken("OBJECT_DEFINITION");
    public static final CeylonToken OR_ASSIGN_OP = new CeylonToken("OR_ASSIGN_OP");
    public static final CeylonToken OR_OP = new CeylonToken("OR_OP");
    public static final CeylonToken OUT = new CeylonToken("OUT");
    public static final CeylonToken OUTER = new CeylonToken("OUTER");
    public static final CeylonToken POWER_OP = new CeylonToken("POWER_OP");
    public static final CeylonToken PRODUCT_OP = new CeylonToken("PRODUCT_OP");
    public static final CeylonToken QMARK = new CeylonToken("QMARK");
    public static final CeylonToken QUOTED_LITERAL = new CeylonToken("QUOTED_LITERAL");
    public static final CeylonToken QuotedLiteralPart = new CeylonToken("QuotedLiteralPart");
    public static final CeylonToken QUOTIENT_OP = new CeylonToken("QUOTIENT_OP");
    public static final CeylonToken RANGE_OP = new CeylonToken("RANGE_OP");
    public static final CeylonToken RBRACE = new CeylonToken("RBRACE");
    public static final CeylonToken RBRACKET = new CeylonToken("RBRACKET");
    public static final CeylonToken REMAINDER_ASSIGN_OP = new CeylonToken("REMAINDER_ASSIGN_OP");
    public static final CeylonToken REMAINDER_OP = new CeylonToken("REMAINDER_OP");
    public static final CeylonToken RETURN = new CeylonToken("RETURN");
    public static final CeylonToken RPAREN = new CeylonToken("RPAREN");
    public static final CeylonToken SAFE_INDEX_OP = new CeylonToken("SAFE_INDEX_OP");
    public static final CeylonToken SAFE_MEMBER_OP = new CeylonToken("SAFE_MEMBER_OP");
    public static final CeylonToken SATISFIES = new CeylonToken("SATISFIES");
    public static final CeylonToken SEMICOLON = new CeylonToken("SEMICOLON");
    public static final CeylonToken SMALL_AS_OP = new CeylonToken("SMALL_AS_OP");
    public static final CeylonToken SMALLER_OP = new CeylonToken("SMALLER_OP");
    public static final CeylonToken SPECIFY = new CeylonToken("SPECIFY");
    public static final CeylonToken SPREAD_OP = new CeylonToken("SPREAD_OP");
    public static final CeylonToken STRING_LITERAL = new CeylonToken("STRING_LITERAL");
    public static final CeylonToken StringPart = new CeylonToken("StringPart");
    public static final CeylonToken SUBTRACT_ASSIGN_OP = new CeylonToken("SUBTRACT_ASSIGN_OP");
    public static final CeylonToken SUM_OP = new CeylonToken("SUM_OP");
    public static final CeylonToken SUPER = new CeylonToken("SUPER");
    public static final CeylonToken SWITCH_CLAUSE = new CeylonToken("SWITCH_CLAUSE");
    public static final CeylonToken THEN_CLAUSE = new CeylonToken("THEN_CLAUSE");
    public static final CeylonToken THIS = new CeylonToken("THIS");
    public static final CeylonToken THROW = new CeylonToken("THROW");
    public static final CeylonToken TRY_CLAUSE = new CeylonToken("TRY_CLAUSE");
    public static final CeylonToken TYPE_CONSTRAINT = new CeylonToken("TYPE_CONSTRAINT");
    public static final CeylonToken UIDENTIFIER = new CeylonToken("UIDENTIFIER");
    public static final CeylonToken UNION_ASSIGN_OP = new CeylonToken("UNION_ASSIGN_OP");
    public static final CeylonToken UNION_OP = new CeylonToken("UNION_OP");
    public static final CeylonToken VALUE_MODIFIER = new CeylonToken("VALUE_MODIFIER");
    public static final CeylonToken VOID_MODIFIER = new CeylonToken("VOID_MODIFIER");
    public static final CeylonToken WHILE_CLAUSE = new CeylonToken("WHILE_CLAUSE");
    public static final CeylonToken WS = new CeylonToken("WS");
    public static final CeylonToken XOR_ASSIGN_OP = new CeylonToken("XOR_ASSIGN_OP");
    public static final CeylonToken XOR_OP = new CeylonToken("XOR_OP");

    public static final TokenSet KEYWORD_SET = TokenSet.create(ABSTRACTED_TYPE, ADAPTED_TYPES, ASSIGN, BREAK, CASE_CLAUSE, CASE_TYPES, CATCH_CLAUSE, CLASS_DEFINITION, CONTINUE, ELSE_CLAUSE, EXISTS, EXTENDS, FINALLY_CLAUSE, FOR_CLAUSE, FUNCTION_MODIFIER, IF_CLAUSE, IMPORT, INTERFACE_DEFINITION, NONEMPTY, OBJECT_DEFINITION, OUT, OUTER, RETURN, SATISFIES, SUPER, SWITCH_CLAUSE, THEN_CLAUSE, THIS, THROW, TRY_CLAUSE, TYPE_CONSTRAINT, VALUE_MODIFIER, VOID_MODIFIER, WHILE_CLAUSE);
    public static final TokenSet WHITE_SPACE_SET = TokenSet.create(WS);
    public static final TokenSet COMMENT_SET = TokenSet.create(MULTI_COMMENT);
    public static final TokenSet STRING_LITERAL_SET = TokenSet.create(STRING_LITERAL);

    private CeylonToken(@NotNull @NonNls String debugName) {
        super(debugName, Ceylon.LANGUAGE);
    }

}
