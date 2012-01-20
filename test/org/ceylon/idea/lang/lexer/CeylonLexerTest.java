package org.ceylon.idea.lang.lexer;

import com.intellij.lexer.Lexer;
import com.intellij.psi.tree.IElementType;
import org.junit.Assert;
import org.junit.Test;

import static org.ceylon.idea.lang.lexer.CeylonToken.*;


public class CeylonLexerTest {

    private Lexer lexer = new CeylonLexerAdapter();

    @Test
    public void testComments() {
        testTokenization("//Comment", LINE_COMMENT);
        testTokenization("//Comment\n", LINE_COMMENT);
        testTokenization("class//Comment", CLASS_DEFINITION, LINE_COMMENT);
        testTokenization("class //Comment", CLASS_DEFINITION, WS, LINE_COMMENT);
    }

    @Test
    public void testLiterals() {
        testTokenization("\"text\"", STRING_LITERAL);
        testTokenization("'text'", QUOTED_LITERAL);
        testTokenization("`t`", CHAR_LITERAL);
    }

    @Test
    public void testCurlyBraces() {
        testTokenization("{", LBRACE);
        testTokenization("}", RBRACE);
        testTokenization("{}", LBRACE, RBRACE);
        testTokenization("{\n}", LBRACE, WS, RBRACE);
    }

    @Test
    public void testBrackets() {
        testTokenization("[", INDEX_OP);
        testTokenization("]", RBRACKET);
        testTokenization("[]", ARRAY);
        testTokenization("[].", ARRAY, MEMBER_OP);
        testTokenization("[\"str\"]", INDEX_OP, STRING_LITERAL, RBRACKET);
    }

    @Test
    public void testKeywords() {
        testTokenization("abstracts", ABSTRACTED_TYPE);
        testTokenization("adapts", ADAPTED_TYPES);
        testTokenization("assign", ASSIGN);
        testTokenization("break", BREAK);
        testTokenization("case", CASE_CLAUSE);
        testTokenization("catch", CATCH_CLAUSE);
        testTokenization("class", CLASS_DEFINITION);
        testTokenization("continue", CONTINUE);
        testTokenization("else", ELSE_CLAUSE);
        testTokenization("exists", EXISTS);
        testTokenization("extends", EXTENDS);
        testTokenization("finally", FINALLY_CLAUSE);
        testTokenization("for", FOR_CLAUSE);
        testTokenization("given", TYPE_CONSTRAINT);
        testTokenization("if", IF_CLAUSE);
        testTokenization("in", IN_OP);
        testTokenization("is", IS_OP);
        testTokenization("satisfies", SATISFIES);
        testTokenization("import", IMPORT);
        testTokenization("interface", INTERFACE_DEFINITION);
        testTokenization("value", VALUE_MODIFIER);
        testTokenization("function", FUNCTION_MODIFIER);
        testTokenization("nonempty", NONEMPTY);
        testTokenization("return", RETURN);
        testTokenization("super", SUPER);
        testTokenization("switch", SWITCH_CLAUSE);
        testTokenization("then", THEN_CLAUSE);
        testTokenization("this", THIS);
        testTokenization("outer", OUTER);
        testTokenization("object", OBJECT_DEFINITION);
        testTokenization("of", CASE_TYPES);
        testTokenization("out", OUT);
        testTokenization("throw", THROW);
        testTokenization("try", TRY_CLAUSE);
        testTokenization("void", VOID_MODIFIER);
        testTokenization("while", WHILE_CLAUSE);
    }


    private void testTokenization(String code, CeylonToken... expectedTokens) {
        lexer.start(code);

        int i = 1;
        for (CeylonToken expectedToken : expectedTokens) {
            IElementType tokenType = lexer.getTokenType();
            Assert.assertEquals("Wrong match at #" + i, expectedToken.getElementType(), tokenType);
            lexer.advance();
            i++;
        }

        //check if the lexer has tokens left
        Assert.assertTrue("Lexer has tokens left: " + lexer.getTokenType(), lexer.getTokenType() == null);
    }
}
