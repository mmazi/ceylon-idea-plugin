package org.ceylon.idea.lang.lexer;

import com.intellij.psi.TokenType;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.TokenSet;


public interface CeylonTokenType extends TokenType {
    IElementType C_STYLE_COMMENT = new CeylonElementType("C_STYLE_COMMENT");
    IElementType STRING_LITERAL = new CeylonElementType("STRING_LITERAL");
    IElementType LIDENTIFIER = new CeylonElementType("LIDENTIFIER");
    IElementType UIDENTIFIER = new CeylonElementType("UIDENTIFIER");

    IElementType ABSTRACTS_KEYWORD = new CeylonElementType("ABSTRACTS_KEYWORD");
    IElementType ADAPTS_KEYWORD = new CeylonElementType("ADAPTS_KEYWORD");
    IElementType ASSIGN_KEYWORD = new CeylonElementType("ASSIGN_KEYWORD");
    IElementType BREAK_KEYWORD = new CeylonElementType("BREAK_KEYWORD");
    IElementType CASE_KEYWORD = new CeylonElementType("CASE_KEYWORD");
    IElementType CATCH_KEYWORD = new CeylonElementType("CATCH_KEYWORD");
    IElementType CLASS_KEYWORD = new CeylonElementType("CLASS_KEYWORD");
    IElementType CONTINUE_KEYWORD = new CeylonElementType("CONTINUE_KEYWORD");
    IElementType ELSE_KEYWORD = new CeylonElementType("ELSE_KEYWORD");
    IElementType EXISTS_KEYWORD = new CeylonElementType("EXISTS_KEYWORD");
    IElementType EXTENDS_KEYWORD = new CeylonElementType("EXTENDS_KEYWORD");
    IElementType FINALLY_KEYWORD = new CeylonElementType("FINALLY_KEYWORD");
    IElementType FOR_KEYWORD = new CeylonElementType("FOR_KEYWORD");
    IElementType FUNCTION_KEYWORD = new CeylonElementType("FUNCTION_KEYWORD");
    IElementType GIVEN_KEYWORD = new CeylonElementType("GIVEN_KEYWORD");
    IElementType IF_KEYWORD = new CeylonElementType("IF_KEYWORD");
    IElementType IMPORT_KEYWORD = new CeylonElementType("IMPORT_KEYWORD");
    IElementType IN_KEYWORD = new CeylonElementType("IN_KEYWORD");
    IElementType INTERFACE_KEYWORD = new CeylonElementType("INTERFACE_KEYWORD");
    IElementType IS_KEYWORD = new CeylonElementType("IS_KEYWORD");
    IElementType NONEMPTY_KEYWORD = new CeylonElementType("NONEMPTY_KEYWORD");
    IElementType OBJECT_KEYWORD = new CeylonElementType("OBJECT_KEYWORD");
    IElementType OF_KEYWORD = new CeylonElementType("OF_KEYWORD");
    IElementType OUT_KEYWORD = new CeylonElementType("OUT_KEYWORD");
    IElementType OUTER_KEYWORD = new CeylonElementType("OUTER_KEYWORD");
    IElementType RETURN_KEYWORD = new CeylonElementType("RETURN_KEYWORD");
    IElementType SATISFIES_KEYWORD = new CeylonElementType("SATISFIES_KEYWORD");
    IElementType SUPER_KEYWORD = new CeylonElementType("SUPER_KEYWORD");
    IElementType SWITCH_KEYWORD = new CeylonElementType("SWITCH_KEYWORD");
    IElementType THEN_KEYWORD = new CeylonElementType("THEN_KEYWORD");
    IElementType THIS_KEYWORD = new CeylonElementType("THIS_KEYWORD");
    IElementType THROW_KEYWORD = new CeylonElementType("THROW_KEYWORD");
    IElementType TRY_KEYWORD = new CeylonElementType("TRY_KEYWORD");
    IElementType VALUE_KEYWORD = new CeylonElementType("VALUE_KEYWORD");
    IElementType VOID_KEYWORD = new CeylonElementType("VOID_KEYWORD");
    IElementType WHILE_KEYWORD = new CeylonElementType("WHILE_KEYWORD");


    TokenSet KEYWORD_SET = TokenSet.create(ABSTRACTS_KEYWORD, ADAPTS_KEYWORD, ASSIGN_KEYWORD, BREAK_KEYWORD, CASE_KEYWORD, CATCH_KEYWORD, CLASS_KEYWORD, CONTINUE_KEYWORD, ELSE_KEYWORD, EXISTS_KEYWORD, EXTENDS_KEYWORD, FINALLY_KEYWORD, FOR_KEYWORD, FUNCTION_KEYWORD, GIVEN_KEYWORD, IF_KEYWORD, IMPORT_KEYWORD, IN_KEYWORD, INTERFACE_KEYWORD, IS_KEYWORD, NONEMPTY_KEYWORD, OBJECT_KEYWORD, OF_KEYWORD, OUT_KEYWORD, OUTER_KEYWORD, RETURN_KEYWORD, SATISFIES_KEYWORD, SUPER_KEYWORD, SWITCH_KEYWORD, THEN_KEYWORD, THIS_KEYWORD, THROW_KEYWORD, TRY_KEYWORD, VALUE_KEYWORD, VOID_KEYWORD, WHILE_KEYWORD);
    TokenSet WHITE_SPACE_SET = TokenSet.create(WHITE_SPACE);
    TokenSet COMMENT_SET = TokenSet.create(C_STYLE_COMMENT);
    TokenSet STRING_LITERAL_SET = TokenSet.create(STRING_LITERAL);

}
