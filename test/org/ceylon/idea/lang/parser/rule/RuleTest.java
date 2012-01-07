package org.ceylon.idea.lang.parser.rule;

import org.ceylon.idea.lang.lexer.CeylonToken;
import org.junit.Test;

import static org.ceylon.idea.lang.lexer.CeylonToken.*;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class RuleTest {
    private final MockPsiBuilder builder = new MockPsiBuilder();

    @Test
    public void testFullPackageName() {
        Rule rule = Rule.FullPackageName;

        valid(rule, LIDENTIFIER);
        valid(rule, LIDENTIFIER, MEMBER_OP, LIDENTIFIER);

        invalid(rule, MEMBER_OP, LIDENTIFIER);
    }

    @Test
    public void testImportElements() {
        // ImportElements:  ImportElement ("," ImportElement)* ("," ImportWildcard)? | ImportWildcard
        Rule rule = Rule.ImportElements;

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
        Rule rule = Rule.Import;

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
    public void testCompilationUnit() {
        // CompilationUnit : (CompilerAnnotation+ ";")? import* (CompilerAnnotations Declaration)*
        Rule rule = Rule.CompilationUnit;

        // <empty>
        valid(rule);

        // CompilerAnnotation+ ";"
        valid(rule, LIDENTIFIER, SEMICOLON);

        // import*
        valid(rule, IMPORT, LIDENTIFIER, LBRACE, RBRACE);
        valid(rule, IMPORT, LIDENTIFIER, LBRACE, RBRACE, IMPORT, LIDENTIFIER, LBRACE, RBRACE);

        // CompilationUnit : CompilerAnnotation+ ";" import
        valid(rule, LIDENTIFIER, SEMICOLON, IMPORT, LIDENTIFIER, LBRACE, RBRACE);

        // import com.example {...}
        valid(rule, IMPORT, WS, LIDENTIFIER, MEMBER_OP, LIDENTIFIER, WS, LBRACE, ELLIPSIS, RBRACE);

        // TODO: Declaration
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
