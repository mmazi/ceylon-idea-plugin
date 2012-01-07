package org.ceylon.idea.lang.parser;

import com.intellij.lang.PsiBuilder;
import com.intellij.psi.tree.IElementType;

import static org.ceylon.idea.lang.lexer.CeylonToken.*;


class ParseHelper {
    private final PsiBuilder builder;

    public ParseHelper(PsiBuilder builder) {
        this.builder = builder;
    }

    /**
     * TODO: Implement
     * {@code
     * annotations : (annotation)*
     * }
     */
    void parseAnnotations() {
    }

    /**
     * {@code Block: "{" (Declaration | Statement)* "}" }
     */
    boolean parseBlock() {
        if (!lookAhead(LBRACE)) {
            return false;
        }

        PsiBuilder.Marker marker = builder.mark();

        parseRequiredToken(LBRACE);

        while (parseDeclarationOrStatement()) {
        }

        parseRequiredToken(RBRACE);

        marker.done(CeylonAstNode.BLOCK);

        return true;
    }

    /**
     * {@code
     * compilationUnit : (compilerAnnotations SEMICOLON)? importList (compilerAnnotations declaration)* EOF
     * }
     */
    void parseCompilationUnit() {

        if (parseCompilerAnnotations()) {
            parseRequiredToken(SEMICOLON);
        }

        parseImportList();

        boolean hasDeclaration;
        do {
            boolean hasCompilerAnnotations = parseCompilerAnnotations();
            hasDeclaration = parseDeclaration();

            if (hasCompilerAnnotations && !hasDeclaration) {
                builder.error("Declaration expected");
            }
        } while (hasDeclaration);
    }

    /**
     * {@code
     * compilerAnnotation : COMPILER_ANNOTATION annotationName (INDEX_OP stringLiteral RBRACKET)?
     * }
     */
    boolean parseCompilerAnnotation() {
        if (!ParserUtils.lookAhead(builder, COMPILER_ANNOTATION)) {
            return false;
        }

        PsiBuilder.Marker marker = builder.mark();

        parseRequiredToken(COMPILER_ANNOTATION);
        parseRequiredToken(LIDENTIFIER);

        if (parseOptionalToken(INDEX_OP)) {
            parseRequiredToken(STRING_LITERAL);
            parseRequiredToken(RBRACKET);
        }

        marker.done(CeylonAstNode.COMPILER_ANNOTATION);
        return true;
    }

    /**
     * {@code
     * compilerAnnotations : (compilerAnnotation)*
     * }
     */

    boolean parseCompilerAnnotations() {

        boolean hasCompilerAnnotations = false;

        while (parseCompilerAnnotation()) {
            hasCompilerAnnotations = true;
        }

        return hasCompilerAnnotations;
    }

    /**
     * TODO: Implement
     * {@code
     * declaration : annotations (objectDeclaration | setterDeclaration | voidOrInferredMethodDeclaration | inferredAttributeDeclaration | typedMethodOrAttributeDeclaration | classDeclaration | interfaceDeclaration)
     * }
     */
    boolean parseDeclaration() {
        parseAnnotations();
        return parseSetterDeclaration();
    }

    /**
     * TODO: Implement
     * {@code
     * declarationOrStatement : compilerAnnotations ((annotatedDeclarationStart) => declaration | statement)
     * }
     */
    boolean parseDeclarationOrStatement() {
        return false;
    }

    /**
     * TODO: Implement
     * {@code
     * erasure : LPAREN (typeName (COMMA typeName)*)? RPAREN
     * }
     */
    void parseErasure() {
    }

    /**
     * {@code Import: "import" FullPackageName "{" ImportElements? "}" }
     */
    boolean parseImport() {
        if (!lookAhead(IMPORT)) {
            return false;
        }

        PsiBuilder.Marker marker = builder.mark();

        parseRequiredToken(IMPORT);
        parseFullPackageName();
        parseRequiredToken(LBRACE);
        parseImportElements();
        parseRequiredToken(RBRACE);

        marker.done(CeylonAstNode.IMPORT);
        return true;
    }

    /**
     * {@code FullPackageName: PackageName ("." PackageName)* }
     * fullPackageName : packageName (MEMBER_OP packageName)*
     */
    void parseFullPackageName() {
        PsiBuilder.Marker marker = builder.mark();

        parsePackageName();

        while (parseOptionalToken(MEMBER_OP)) {
            parsePackageName();
        }

        marker.done(CeylonAstNode.IMPORT_PATH);
    }

    /**
     * {@code
     * importElement : compilerAnnotations (memberAlias? memberName erasure? | typeAlias? typeName erasure? (mportElementList?)
     * }
     */
    boolean parseImportElement() {
        PsiBuilder.Marker marker = builder.mark();
        parseCompilerAnnotations();

        if (lookAhead(LIDENTIFIER)) {
            parseMemberAlias();
            parseMemberName();
            parseErasure();
        } else if (lookAhead(UIDENTIFIER)) {
            parseTypeAlias();
            parseTypeName();
            parseErasure();
        } else {
            marker.rollbackTo();
            return false;
        }


        marker.done(CeylonAstNode.IMPORT_MEMBER_OR_TYPE);
        return true;
    }

    /**
     * {@code
     * importElementList : LBRACE ((importElement (COMMA importElement)* (COMMA importWildcard)?) | importWildcard)? RBRACE
     * }
     */
    void parseImportElements() {
        PsiBuilder.Marker marker = builder.mark();

        if (!parseImportWildcard()) {
            if (parseImportElement()) {
                while (parseOptionalToken(COMMA)) {
                    if (parseImportWildcard()) {
                        break;
                    } else {
                        parseImportElement();
                    }
                }
            }
        }

        marker.done(CeylonAstNode.IMPORT_MEMBER_OR_TYPE_LIST);
    }

    /**
     * {@code ImportList: Import* }
     */
    void parseImportList() {
        if (!ParserUtils.lookAhead(builder, IMPORT)) {
            return;
        }

        PsiBuilder.Marker marker = builder.mark();

        while (parseImport()) {
        }

        marker.done(CeylonAstNode.IMPORT_LIST);
    }

    /**
     * {@code
     * importWildcard : ELLIPSIS
     * }
     */
    boolean parseImportWildcard() {
        if (!ParserUtils.lookAhead(builder, ELLIPSIS)) {
            return false;
        }

        PsiBuilder.Marker marker = builder.mark();

        parseRequiredToken(ELLIPSIS);

        marker.done(CeylonAstNode.IMPORT_WILDCARD);
        return true;
    }

    /**
     * {@code
     * memberAlias : memberName SPECIFY
     * }
     */
    void parseMemberAlias() {
        PsiBuilder.Marker marker = builder.mark();
        parseMemberName();
        if (parseOptionalToken(SPECIFY)) {
            marker.done(CeylonAstNode.ALIAS);
        } else {
            marker.rollbackTo();
        }
    }

    /**
     * TODO: Implement
     * {@code
     * memberName : LIDENTIFIER
     * }
     */
    void parseMemberName() {
        parseRequiredToken(LIDENTIFIER);
    }

    /**
     * {@code
     * packageName : LIDENTIFIER
     * }
     */
    void parsePackageName() {
        parseRequiredToken(LIDENTIFIER);
    }

    /**
     * {@code
     * setterDeclaration : ASSIGN memberNameDeclaration (block | SEMICOLON)
     * }
     */
    boolean parseSetterDeclaration() {
        if (!lookAhead(ASSIGN)) {
            return false;
        }

        PsiBuilder.Marker marker = builder.mark();

        parseRequiredToken(ASSIGN);
        parseMemberName();

        if (!parseBlock()) {
            parseRequiredToken(SEMICOLON, "block or SEMICOLON expected");
        }

        marker.done(CeylonAstNode.ATTRIBUTE_SETTER_DEFINITION);
        return true;
    }

    /**
     * {@code
     * typeAlias : typeName SPECIFY
     * }
     */
    void parseTypeAlias() {
        PsiBuilder.Marker marker = builder.mark();
        parseTypeName();
        if (parseOptionalToken(SPECIFY)) {
            marker.done(CeylonAstNode.ALIAS);
        } else {
            marker.rollbackTo();
        }
    }

    /**
     * TODO: Implement
     * {@code
     * typeName : UIDENTIFIER
     * }
     */
    void parseTypeName() {
        parseRequiredToken(UIDENTIFIER);
    }


    private boolean parseOptionalToken(IElementType elem) {
        return ParserUtils.getToken(builder, elem);
    }

    private boolean parseRequiredToken(IElementType elem) {
        return parseRequiredToken(elem, elem + " expected");
    }

    private boolean parseRequiredToken(IElementType elem, String errorMsg) {
        return ParserUtils.getToken(builder, elem, errorMsg);
    }


    private boolean lookAhead(IElementType... elems) {
        return ParserUtils.lookAhead(builder, elems);
    }

}
