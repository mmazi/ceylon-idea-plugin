package org.ceylon.idea.lang.parser;

import org.ceylon.idea.lang.lexer.CeylonToken;
import org.ceylon.idea.lang.parser.rule.MockPsiBuilder;
import org.ceylon.idea.lang.parser.rule.Rule;
import org.junit.Test;

import static org.ceylon.idea.lang.lexer.CeylonToken.*;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class RuleTest {
    private final MockPsiBuilder builder = new MockPsiBuilder();

    @Test
    public void testFullPackageName() {
        Rule rule = ParserRules.FullPackageName;

        valid(rule, LIDENTIFIER);
        valid(rule, LIDENTIFIER, MEMBER_OP, LIDENTIFIER);

        invalid(rule, MEMBER_OP, LIDENTIFIER);
    }

    @Test
    public void testImportElements() {
        // ImportElements:  ImportElement ("," ImportElement)* ("," ImportWildcard)? | ImportWildcard
        Rule rule = ParserRules.ImportElements;

        // ImportElement
        valid(rule, UIDENTIFIER);

        // ImportElement "," ImportElement
        valid(rule, UIDENTIFIER, COMMA, UIDENTIFIER);

        // ImportElement "," ImportWildcard
        valid(rule, LIDENTIFIER, COMMA, ELLIPSIS);

        //  ImportElement "," ImportElement "," ImportWildcard
        valid(rule, LIDENTIFIER, COMMA, UIDENTIFIER, COMMA, ELLIPSIS);

        // ImportWildcard
        valid(rule, ELLIPSIS);
    }

    @Test
    public void testImport() {
        // Import:  "import" FullPackageName "{" ImportElements? "}"
        Rule rule = ParserRules.Import;

        // Import:  "import" FullPackageName "{" "}"
        valid(rule, IMPORT, LIDENTIFIER, LBRACE, RBRACE);

        // Import:  "import" FullPackageName "{" ImportElements? "}"
        valid(rule, IMPORT, LIDENTIFIER, LBRACE, ELLIPSIS, RBRACE);
        valid(rule, IMPORT, LIDENTIFIER, LBRACE, LIDENTIFIER, RBRACE);
        valid(rule, IMPORT, LIDENTIFIER, LBRACE, UIDENTIFIER, RBRACE);

        invalid(rule, IMPORT, UIDENTIFIER, LBRACE, RBRACE);
        invalid(rule, IMPORT, LIDENTIFIER, LBRACE, UIDENTIFIER, UIDENTIFIER, RBRACE);
        invalid(rule, IMPORT, LIDENTIFIER, UIDENTIFIER);
        invalid(rule, IMPORT, LIDENTIFIER, LIDENTIFIER, LBRACE, UIDENTIFIER, RBRACE);
    }

    @Test
    public void testAttributeSetter() {
        // AttributeSetter:  "assign" MemberName Block
        Rule rule = ParserRules.AttributeSetter;

        valid(rule, ASSIGN, LIDENTIFIER, LBRACE, RBRACE);
    }

    @Test
    public void testAttributeGetter() {
        // AttributeGetter:   (UnionType | "value") MemberName Block
        Rule rule = ParserRules.AttributeGetter;

        valid(rule, VALUE_MODIFIER, LIDENTIFIER, LBRACE, RBRACE);
        valid(rule, UIDENTIFIER, ARRAY, LIDENTIFIER, LBRACE, RBRACE);
        valid(rule, UIDENTIFIER, UNION_OP, UIDENTIFIER, QMARK, LIDENTIFIER, LBRACE, RBRACE);
    }

    @Test
    public void testAnnotation() {
        // MemberName ( Arguments | Literal+ )?
        Rule rule = ParserRules.Annotation;

        valid(rule, LIDENTIFIER);
        valid(rule, LIDENTIFIER, STRING_LITERAL);
        valid(rule, LIDENTIFIER, STRING_LITERAL, STRING_LITERAL);
        valid(rule, LIDENTIFIER, LBRACE, RBRACE);
    }

    @Test
    public void testCompilationUnit() {
        // CompilationUnit : (CompilerAnnotation+ ";")? import* (CompilerAnnotations Declaration)*
        Rule rule = ParserRules.CompilationUnit;

        // <empty>
        valid(rule);

        // CompilerAnnotation+ ";"
        valid(rule, COMPILER_ANNOTATION, LIDENTIFIER, SEMICOLON);

        // import*
        valid(rule, IMPORT, LIDENTIFIER, LBRACE, RBRACE);
        valid(rule, IMPORT, LIDENTIFIER, LBRACE, RBRACE, IMPORT, LIDENTIFIER, LBRACE, RBRACE);

        // CompilationUnit : CompilerAnnotation+ ";" import
        valid(rule, COMPILER_ANNOTATION, LIDENTIFIER, SEMICOLON, IMPORT, LIDENTIFIER, LBRACE, RBRACE);

        // import com.example {...}
        valid(rule, IMPORT, LIDENTIFIER, MEMBER_OP, LIDENTIFIER, LBRACE, ELLIPSIS, RBRACE);

        // TODO: Declaration
    }

    private void valid(Rule rule, CeylonToken... tokens) {
        builder.setTokens(tokens);
        assertTrue(rule.parseRequired(builder));
        assertTrue(builder.eof());
    }

    private void invalid(Rule rule, CeylonToken... tokens) {
        builder.setTokens(tokens);
        assertFalse(rule.parseRequired(builder));
    }

}
