package org.ceylon.idea.lang.parser.rule;

import org.ceylon.idea.lang.lexer.CeylonToken;
import org.junit.Test;

import static org.ceylon.idea.lang.lexer.CeylonToken.*;
import static org.ceylon.idea.lang.parser.rule.Rules.*;
import static org.junit.Assert.*;

public class ParserEngineTest {

    private final MockPsiBuilder builder = new MockPsiBuilder();

    @Test
    public void testToken() {
        Rule rule = COMMA;

        valid(rule, COMMA);

        invalid(rule, SEMICOLON);
    }

    @Test
    public void testAnyToken() {
        Rule rule = any(COMMA, SEMICOLON);

        valid(rule, COMMA, IMPORT);
        valid(rule, SEMICOLON, IMPORT);

        invalid(rule, IMPORT, COMMA);
    }


    @Test
    public void testZeroOrOneToken() {
        Rule rule = zeroOrOne(SEMICOLON);

        valid(rule, COMMA, IMPORT);
        assertEquals(COMMA, builder.getToken());

        valid(rule, SEMICOLON, IMPORT);
        assertEquals(IMPORT, builder.getToken());
    }

    @Test
    public void testOneOrMoreToken() {
        Rule rule = oneOrMore(COMMA);

        valid(rule, COMMA, SEMICOLON);
        valid(rule, COMMA, COMMA);
        invalid(rule, SEMICOLON, SEMICOLON);
    }

    @Test
    public void testZeroOrMoreToken() {
        Rule rule = zeroOrMore(SEMICOLON);

        valid(rule, COMMA, IMPORT);
        assertEquals(COMMA, builder.getToken());

        valid(rule, SEMICOLON, IMPORT);
        assertEquals(IMPORT, builder.getToken());

        valid(rule, SEMICOLON, SEMICOLON, IMPORT);
        assertEquals(IMPORT, builder.getToken());
    }

    @Test
    public void testSequence() {
        Rule rule = sequence(COMMA, SEMICOLON);

        valid(rule, COMMA, SEMICOLON);
        invalid(rule, SEMICOLON, COMMA);
    }

    @Test
    public void testZeroOrOneSequence() {
        Rule rule = zeroOrOne(LBRACE, RBRACE);

        valid(rule, SEMICOLON);
        assertEquals(SEMICOLON, builder.getToken());

        valid(rule, LBRACE, RBRACE, SEMICOLON);
        assertEquals(SEMICOLON, builder.getToken());

        valid(rule, LBRACE, RBRACE, LBRACE, RBRACE, SEMICOLON);
        assertEquals(LBRACE, builder.getToken());
    }

    @Test
    public void testZeroOrMoreSequence() {
        Rule rule = zeroOrMore(LBRACE, RBRACE);

        valid(rule, SEMICOLON);
        assertEquals(SEMICOLON, builder.getToken());

        valid(rule, LBRACE, RBRACE, SEMICOLON);
        assertEquals(SEMICOLON, builder.getToken());

        valid(rule, LBRACE, RBRACE, LBRACE, RBRACE, SEMICOLON);
        assertEquals(SEMICOLON, builder.getToken());
    }


    @Test
    public void testComplex() {
        Rule rule = new ComplexRule("rule").one(COMMA).oneOrMore(SEMICOLON);

        valid(rule, COMMA, SEMICOLON);
        valid(rule, COMMA, SEMICOLON, SEMICOLON);
        invalid(rule, COMMA);
        invalid(rule, SEMICOLON);
        invalid(rule, SEMICOLON, COMMA);
    }

    @Test
    public void testComplexSequence() {
        Rule rule = new ComplexRule("rule").one(sequence(COMMA, SEMICOLON));

        valid(rule, COMMA, SEMICOLON);
        valid(rule, COMMA, SEMICOLON, LBRACE);
        invalid(rule, COMMA);
        invalid(rule, SEMICOLON);
        invalid(rule, SEMICOLON, COMMA);
    }

    @Test
    public void testZeroOrAny() {
        Rule rule = Rules.zeroOrAny(COMMA, SEMICOLON);

        valid(rule);

        valid(rule, COMMA);
        assertTrue(builder.eof());

        valid(rule, SEMICOLON);
        assertTrue(builder.eof());

        valid(rule, IMPORT);
        assertFalse(builder.eof());
    }

    private void valid(Rule rule, CeylonToken... tokens) {
        builder.setTokens(tokens);
        assertTrue(rule.parseRequired(builder));
    }

    private void invalid(Rule rule, CeylonToken... tokens) {
        builder.setTokens(tokens);
        assertFalse(rule.parseRequired(builder));
    }

}