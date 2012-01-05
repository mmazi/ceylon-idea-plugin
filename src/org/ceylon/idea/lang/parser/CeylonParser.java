package org.ceylon.idea.lang.parser;

import com.intellij.lang.ASTNode;
import com.intellij.lang.PsiBuilder;
import com.intellij.lang.PsiParser;
import com.intellij.psi.tree.IElementType;
import org.ceylon.idea.lang.lexer.CeylonToken;
import org.jetbrains.annotations.NotNull;

import static org.ceylon.idea.lang.parser.ParserUtils.getToken;
import static org.ceylon.idea.lang.parser.ParserUtils.lookAhead;

@SuppressWarnings("unused")
public class CeylonParser implements PsiParser {
    @NotNull
    @Override
    public ASTNode parse(IElementType root, PsiBuilder builder) {
        builder.setDebugMode(true);

        PsiBuilder.Marker rootMarker = builder.mark();

        parseCompilationUnit(builder);

        while (!builder.eof()) {
            builder.advanceLexer();
        }

        rootMarker.done(root);

        return builder.getTreeBuilt();
    }

    /**
     * TODO: Implement
     * {@code
     * abbreviatedType : qualifiedType (DEFAULT_OP | ARRAY)*
     * }
     */
    void parseAbbreviatedType(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * abstractedType : ABSTRACTED_TYPE qualifiedType
     * }
     */
    void parseAbstractedType(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * adaptedTypes : ADAPTED_TYPES qualifiedType (INTERSECTION_OP (qualifiedType ))*
     * }
     */
    void parseAdaptedTypes(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * additiveExpression : multiplicativeExpression (additiveOperator multiplicativeExpression)*
     * }
     */
    void parseAdditiveExpression(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * additiveOperator : SUM_OP | DIFFERENCE_OP | UNION_OP | XOR_OP | COMPLEMENT_OP
     * }
     */
    void parseAdditiveOperator(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * annotatedDeclarationStart : annotation* declarationStart
     * }
     */
    void parseAnnotatedDeclarationStart(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * annotation : annotationName annotationArguments
     * }
     */
    void parseAnnotation(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * annotationArguments : arguments | literalArguments
     * }
     */
    void parseAnnotationArguments(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * annotationName : LIDENTIFIER
     * }
     */
    void parseAnnotationName(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * annotations : (annotation)*
     * }
     */
    void parseAnnotations(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * arguments : positionalArguments | namedArguments
     * }
     */
    void parseArguments(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * assignmentExpression : thenElseExpression (assignmentOperator assignmentExpression)?
     * }
     */
    void parseAssignmentExpression(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * assignmentOperator : ASSIGN_OP | ADD_ASSIGN_OP | SUBTRACT_ASSIGN_OP | MULTIPLY_ASSIGN_OP | DIVIDE_ASSIGN_OP | REMAINDER_ASSIGN_OP | INTERSECT_ASSIGN_OP | UNION_ASSIGN_OP | XOR_ASSIGN_OP | COMPLEMENT_ASSIGN_OP | AND_ASSIGN_OP | OR_ASSIGN_OP
     * }
     */
    void parseAssignmentOperator(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * attributeBody[StaticType type] : (namedArguments) => namedArguments | block
     * }
     */
    void parseAttributeBody(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * base : nonstringLiteral | stringExpression | enumeration | selfReference | typeReference | memberReference | parExpression
     * }
     */
    void parseBase(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * block : LBRACE (declarationOrStatement)* RBRACE
     * }
     */
    boolean parseBlock(PsiBuilder builder) {
        if (!ParserUtils.lookAhead(builder, CeylonToken.LBRACE)) {
            return false;
        }

        PsiBuilder.Marker marker = builder.mark();

        getToken(builder, CeylonToken.LBRACE);

        while (parseDeclarationOrStatement(builder)) {
        }

        getToken(builder, CeylonToken.RBRACE, "RBRACE expected");

        marker.done(CeylonAstNode.BLOCK);

        return true;
    }

    /**
     * TODO: Implement
     * {@code
     * booleanCondition : LPAREN (expression)? RPAREN
     * }
     */
    void parseBooleanCondition(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * breakDirective : BREAK
     * }
     */
    void parseBreakDirective(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * caseBlock : CASE_CLAUSE LPAREN caseItem RPAREN block
     * }
     */
    void parseCaseBlock(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * caseItem : (IS_OP)=>isCaseCondition | (SATISFIES)=>satisfiesCaseCondition | matchCaseCondition
     * }
     */
    void parseCaseItem(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * cases : (caseBlock)+ (defaultCaseBlock)?
     * }
     */
    void parseCases(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * caseType : qualifiedType | memberName
     * }
     */
    void parseCaseType(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * caseTypes : CASE_TYPES caseType (UNION_OP (caseType ))*
     * }
     */
    void parseCaseTypes(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * catchBlock : CATCH_CLAUSE catchVariable block
     * }
     */
    void parseCatchBlock(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * catchVariable : LPAREN (variable)? RPAREN
     * }
     */
    void parseCatchVariable(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * classBody : LBRACE (declarationOrStatement)* RBRACE
     * }
     */
    void parseClassBody(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * classDeclaration : CLASS_DEFINITION typeNameDeclaration (typeParameters)? (parameters)? (caseTypes)? (extendedType)? (satisfiedTypes)? (typeConstraints)? (classBody | (typeSpecifier)? SEMICOLON)
     * }
     */
    void parseClassDeclaration(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * comparisonExpression : existenceEmptinessExpression (comparisonOperator existenceEmptinessExpression | typeOperator qualifiedType)? | typeOperator qualifiedType existenceEmptinessExpression
     * }
     */
    void parseComparisonExpression(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * comparisonOperator : COMPARE_OP | SMALL_AS_OP | LARGE_AS_OP | LARGER_OP | SMALLER_OP | IN_OP
     * }
     */
    void parseComparisonOperator(PsiBuilder builder) {
    }

    /**
     * {@code
     * compilationUnit : (compilerAnnotations SEMICOLON)? importList (compilerAnnotations declaration)* EOF
     * }
     */
    void parseCompilationUnit(PsiBuilder builder) {

        if (parseCompilerAnnotations(builder)) {
            getToken(builder, CeylonToken.SEMICOLON, "SEMICOLON expected");
        }

        parseImportList(builder);

        boolean hasDeclaration;
        do {
            boolean hasCompilerAnnotations = parseCompilerAnnotations(builder);
            hasDeclaration = parseDeclaration(builder);

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
    boolean parseCompilerAnnotation(PsiBuilder builder) {
        if (!ParserUtils.lookAhead(builder, CeylonToken.COMPILER_ANNOTATION)) {
            return false;
        }

        PsiBuilder.Marker marker = builder.mark();

        getToken(builder, CeylonToken.COMPILER_ANNOTATION);
        getToken(builder, CeylonToken.LIDENTIFIER, "LIDENTIFIER expected");

        if (getToken(builder, CeylonToken.INDEX_OP)) {
            getToken(builder, CeylonToken.STRING_LITERAL, "STRING_LITERAL expected");
            getToken(builder, CeylonToken.RBRACKET, "RBRACKET expected");
        }

        marker.done(CeylonAstNode.COMPILER_ANNOTATION);
        return true;
    }

    /**
     * {@code
     * compilerAnnotations : (compilerAnnotation)*
     * }
     */

    boolean parseCompilerAnnotations(PsiBuilder builder) {

        boolean hasCompilerAnnotations = false;

        while (parseCompilerAnnotation(builder)) {
            hasCompilerAnnotations = true;
        }

        return hasCompilerAnnotations;
    }

    /**
     * TODO: Implement
     * {@code
     * comprehension : forComprehensionClause
     * }
     */
    void parseComprehension(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * comprehensionClause : forComprehensionClause | ifComprehensionClause | expressionComprehensionClause
     * }
     */
    void parseComprehensionClause(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * condition : (LPAREN EXISTS)=>existsCondition | (LPAREN NONEMPTY)=>nonemptyCondition | (LPAREN IS_OP)=>isCondition | (LPAREN SATISFIES)=>satisfiesCondition | booleanCondition
     * }
     */
    void parseCondition(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * conjunctionExpression : logicalNegationExpression (conjunctionOperator logicalNegationExpression)*
     * }
     */
    void parseConjunctionExpression(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * conjunctionOperator : AND_OP
     * }
     */
    void parseConjunctionOperator(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * containment : IN_OP (expression)?
     * }
     */
    void parseContainment(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * continueDirective : CONTINUE
     * }
     */
    void parseContinueDirective(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * controlBlock : ((LBRACE)=> block )
     * }
     */
    void parseControlBlock(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * controlStatement : ifElse | switchCaseElse | whileLoop | forElse | tryCatchFinally
     * }
     */
    void parseControlStatement(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * declaration : annotations (objectDeclaration | setterDeclaration | voidOrInferredMethodDeclaration | inferredAttributeDeclaration | typedMethodOrAttributeDeclaration | classDeclaration | interfaceDeclaration)
     * }
     */
    boolean parseDeclaration(PsiBuilder builder) {
        parseAnnotations(builder);
        return parseSetterDeclaration(builder);
    }

    /**
     * TODO: Implement
     * {@code
     * declarationKeyword : VALUE_MODIFIER | FUNCTION_MODIFIER | ASSIGN | VOID_MODIFIER | INTERFACE_DEFINITION | CLASS_DEFINITION | OBJECT_DEFINITION
     * }
     */
    void parseDeclarationKeyword(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * declarationOrStatement : compilerAnnotations ((annotatedDeclarationStart) => declaration | statement)
     * }
     */
    boolean parseDeclarationOrStatement(PsiBuilder builder) {
        return false;
    }

    /**
     * TODO: Implement
     * {@code
     * declarationStart : declarationKeyword | type (ELLIPSIS | LIDENTIFIER)
     * }
     */
    void parseDeclarationStart(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * defaultCaseBlock : ELSE_CLAUSE block
     * }
     */
    void parseDefaultCaseBlock(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * defaultExpression : negationComplementExpression (defaultOperator negationComplementExpression)*
     * }
     */
    void parseDefaultExpression(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * defaultOperator : DEFAULT_OP
     * }
     */
    void parseDefaultOperator(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * directive : returnDirective | throwDirective | breakDirective | continueDirective
     * }
     */
    void parseDirective(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * directiveStatement : directive SEMICOLON
     * }
     */
    void parseDirectiveStatement(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * disjunctionExpression : conjunctionExpression (disjunctionOperator conjunctionExpression)*
     * }
     */
    void parseDisjunctionExpression(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * disjunctionOperator : OR_OP
     * }
     */
    void parseDisjunctionOperator(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * elementSelectionOperator : SAFE_INDEX_OP | INDEX_OP
     * }
     */
    void parseElementSelectionOperator(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * elseBlock : ELSE_CLAUSE (elseIf | block)
     * }
     */
    void parseElseBlock(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * elseIf : ifElse
     * }
     */
    void parseElseIf(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * entryType : abbreviatedType (ENTRY_OP (abbreviatedType ))?
     * }
     */
    void parseEntryType(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * enumeration : LBRACE (expressions | comprehension)? RBRACE
     * }
     */
    void parseEnumeration(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * equalityExpression : comparisonExpression (equalityOperator comparisonExpression)?
     * }
     */
    void parseEqualityExpression(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * equalityOperator : EQUAL_OP | NOT_EQUAL_OP | IDENTICAL_OP
     * }
     */
    void parseEqualityOperator(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * erasure : LPAREN (typeName (COMMA typeName)*)? RPAREN
     * }
     */
    void parseErasure(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * existenceEmptinessExpression : rangeIntervalEntryExpression (existsNonemptyOperator)? | existsNonemptyOperator rangeIntervalEntryExpression
     * }
     */
    void parseExistenceEmptinessExpression(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * existsCondition : (LPAREN EXISTS LIDENTIFIER RPAREN) => LPAREN EXISTS impliedVariable RPAREN | (LPAREN EXISTS compilerAnnotations (declarationStart|specificationStart)) => LPAREN EXISTS specifiedVariable RPAREN | LPAREN EXISTS (expression)? RPAREN
     * }
     */
    void parseExistsCondition(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * existsNonemptyOperator : EXISTS | NONEMPTY
     * }
     */
    void parseExistsNonemptyOperator(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * exponentiationExpression : incrementDecrementExpression (exponentiationOperator exponentiationExpression)?
     * }
     */
    void parseExponentiationExpression(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * exponentiationOperator : POWER_OP
     * }
     */
    void parseExponentiationOperator(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * expression : assignmentExpression
     * }
     */
    void parseExpression(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * expressionComprehensionClause : expression |
     * }
     */
    void parseExpressionComprehensionClause(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * expressionOrSpecificationStatement : expression (specifier)? (SEMICOLON | COMMA)
     * }
     */
    void parseExpressionOrSpecificationStatement(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * expressions : expression (COMMA (expression ))*
     * }
     */
    void parseExpressions(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * extendedType : EXTENDS (qualifiedType | SUPER MEMBER_OP typeReference) (positionalArguments )
     * }
     */
    void parseExtendedType(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * failBlock : ELSE_CLAUSE block
     * }
     */
    void parseFailBlock(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * finallyBlock : FINALLY_CLAUSE block
     * }
     */
    void parseFinallyBlock(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * forBlock : FOR_CLAUSE forIterator controlBlock
     * }
     */
    void parseForBlock(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * forComprehensionClause : FOR_CLAUSE forIterator comprehensionClause
     * }
     */
    void parseForComprehensionClause(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * forElse : forBlock (failBlock)?
     * }
     */
    void parseForElse(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * forIterator : LPAREN compilerAnnotations (var (containment | ENTRY_OP var containment)?)? RPAREN
     * }
     */
    void parseForIterator(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * ifBlock : IF_CLAUSE condition controlBlock
     * }
     */
    void parseIfBlock(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * ifComprehensionClause : IF_CLAUSE condition comprehensionClause
     * }
     */
    void parseIfComprehensionClause(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * ifElse : ifBlock (elseBlock)?
     * }
     */
    void parseIfElse(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * impliedVariable : memberName
     * }
     */
    void parseImpliedVariable(PsiBuilder builder) {
    }

    /**
     * {@code
     * importDeclaration : IMPORT fullPackageName importElementList
     * }
     */
    boolean parseImportDeclaration(PsiBuilder builder) {
        if (!lookAhead(builder, CeylonToken.IMPORT)) {
            return false;
        }

        PsiBuilder.Marker marker = builder.mark();

        getToken(builder, CeylonToken.IMPORT);

        parseFullPackageName(builder);

        parseImportElementList(builder);

        marker.done(CeylonAstNode.IMPORT);
        return true;
    }

    /**
     * {@code
     * fullPackageName : packageName (MEMBER_OP packageName)*
     * }
     */
    void parseFullPackageName(PsiBuilder builder) {
        PsiBuilder.Marker marker = builder.mark();

        parsePackageName(builder);

        while (getToken(builder, CeylonToken.MEMBER_OP)) {
            parsePackageName(builder);
        }

        marker.done(CeylonAstNode.IMPORT_PATH);
    }

    /**
     * {@code
     * importElement : compilerAnnotations (memberAlias? memberName erasure? | typeAlias? typeName erasure? (mportElementList?)
     * }
     */
    boolean parseImportElement(PsiBuilder builder) {
        PsiBuilder.Marker marker = builder.mark();
        parseCompilerAnnotations(builder);

        if (lookAhead(builder, CeylonToken.LIDENTIFIER)) {
            parseMemberAlias(builder);
            parseMemberName(builder);
            parseErasure(builder);
        } else if (lookAhead(builder, CeylonToken.UIDENTIFIER)) {
            parseTypeAlias(builder);
            parseTypeName(builder);
            parseErasure(builder);
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
    void parseImportElementList(PsiBuilder builder) {
        PsiBuilder.Marker marker = builder.mark();

        getToken(builder, CeylonToken.LBRACE, "LBRACE expected");

        if (!parseImportWildcard(builder)) {
            if (parseImportElement(builder)) {
                while (getToken(builder, CeylonToken.COMMA)) {
                    if (parseImportWildcard(builder)) {
                        break;
                    } else {
                        parseImportElement(builder);
                    }
                }
            }
        }

        getToken(builder, CeylonToken.RBRACE, "RBRACE expected");

        marker.done(CeylonAstNode.IMPORT_MEMBER_OR_TYPE_LIST);
    }

    /**
     * {@code
     * importList : (importDeclaration)*
     * }
     */
    void parseImportList(PsiBuilder builder) {
        if (!ParserUtils.lookAhead(builder, CeylonToken.IMPORT)) {
            return;
        }

        PsiBuilder.Marker marker = builder.mark();

        while (parseImportDeclaration(builder)) {
        }

        marker.done(CeylonAstNode.IMPORT_LIST);
    }

    /**
     * {@code
     * importWildcard : ELLIPSIS
     * }
     */
    boolean parseImportWildcard(PsiBuilder builder) {
        if (!ParserUtils.lookAhead(builder, CeylonToken.ELLIPSIS)) {
            return false;
        }

        PsiBuilder.Marker marker = builder.mark();

        ParserUtils.getToken(builder, CeylonToken.ELLIPSIS);

        marker.done(CeylonAstNode.IMPORT_WILDCARD);
        return true;
    }

    /**
     * TODO: Implement
     * {@code
     * incrementDecrementExpression : prefixOperator incrementDecrementExpression | postfixIncrementDecrementExpression
     * }
     */
    void parseIncrementDecrementExpression(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * index : additiveExpression
     * }
     */
    void parseIndex(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * indexExpression : elementSelectionOperator indexOrIndexRange RBRACKET
     * }
     */
    void parseIndexExpression(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * indexOrIndexRange : index (| '...' | '..' index)
     * }
     */
    void parseIndexOrIndexRange(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * inferredAttributeDeclaration : VALUE_MODIFIER memberNameDeclaration ((specifier | initializer)? SEMICOLON | block)
     * }
     */
    void parseInferredAttributeDeclaration(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * inferredGetterArgument : VALUE_MODIFIER memberNameDeclaration block
     * }
     */
    void parseInferredGetterArgument(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * initializer : ASSIGN_OP expression
     * }
     */
    void parseInitializer(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * inlineFunctionalArgument : memberName ((parametersStart) => parameters)? (LPAREN expression RPAREN | block)
     * }
     */
    void parseInlineFunctionalArgument(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * interfaceBody : LBRACE (declarationOrStatement)* RBRACE
     * }
     */
    void parseInterfaceBody(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * interfaceDeclaration : INTERFACE_DEFINITION typeNameDeclaration (typeParameters)? (caseTypes)? (adaptedTypes)? (satisfiedTypes)? (typeConstraints)? (interfaceBody | (typeSpecifier)? SEMICOLON)
     * }
     */
    void parseInterfaceDeclaration(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * interpolatedExpressionStart : LPAREN | LBRACE | LIDENTIFIER | UIDENTIFIER | selfReference | nonstringLiteral | prefixOperatorStart
     * }
     */
    void parseInterpolatedExpressionStart(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * intersectionType : entryType ((INTERSECTION_OP (entryType ))+)?
     * }
     */
    void parseIntersectionType(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * isCaseCondition : IS_OP type
     * }
     */
    void parseIsCaseCondition(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * isCondition : (LPAREN IS_OP type LIDENTIFIER RPAREN) => LPAREN IS_OP type impliedVariable RPAREN | (LPAREN IS_OP type LIDENTIFIER SPECIFY) => LPAREN IS_OP (type memberName specifier)? RPAREN | LPAREN IS_OP abbreviatedType (expression)? RPAREN
     * }
     */
    void parseIsCondition(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * literalArgument : nonstringLiteral | stringLiteral
     * }
     */
    void parseLiteralArgument(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * literalArguments : (literalArgument)*
     * }
     */
    void parseLiteralArguments(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * logicalNegationExpression : notOperator logicalNegationExpression | equalityExpression
     * }
     */
    void parseLogicalNegationExpression(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * matchCaseCondition : expressions
     * }
     */
    void parseMatchCaseCondition(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * memberAlias : memberNameDeclaration SPECIFY
     * }
     */
    void parseMemberAlias(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * memberName : LIDENTIFIER
     * }
     */
    void parseMemberName(PsiBuilder builder) {
        getToken(builder, CeylonToken.LIDENTIFIER, "LIDENTIFIER expected");
    }

    /**
     * TODO: Implement
     * {@code
     * memberNameDeclaration : memberName | typeName
     * }
     */
    void parseMemberNameDeclaration(PsiBuilder builder) {
        parseMemberName(builder);
        parseTypeName(builder);
    }

    /**
     * TODO: Implement
     * {@code
     * memberReference : memberName ((typeArgumentsStart) => typeArguments)?
     * }
     */
    void parseMemberReference(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * memberSelectionOperator : MEMBER_OP | SAFE_MEMBER_OP | SPREAD_OP
     * }
     */
    void parseMemberSelectionOperator(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * methodBody[StaticType type] : (namedArguments) => namedArguments | block
     * }
     */
    void parseMethodBody(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * multiplicativeExpression : defaultExpression (multiplicativeOperator defaultExpression)*
     * }
     */
    void parseMultiplicativeExpression(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * multiplicativeOperator : PRODUCT_OP | QUOTIENT_OP | REMAINDER_OP | INTERSECTION_OP
     * }
     */
    void parseMultiplicativeOperator(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * namedArgument : compilerAnnotations (namedSpecifiedArgument | namedArgumentDeclaration)
     * }
     */
    void parseNamedArgument(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * namedArgumentDeclaration : objectArgument | typedMethodOrGetterArgument | voidOrInferredMethodArgument | inferredGetterArgument
     * }
     */
    void parseNamedArgumentDeclaration(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * namedArguments : LBRACE ((namedArgumentStart) => namedArgument)* (sequencedArgument | comprehension)? RBRACE
     * }
     */
    void parseNamedArguments(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * namedArgumentStart : compilerAnnotation* (specificationStart | declarationStart)
     * }
     */
    void parseNamedArgumentStart(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * namedSpecifiedArgument : memberNameDeclaration specifier SEMICOLON
     * }
     */
    void parseNamedSpecifiedArgument(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * negationComplementExpression : unaryMinusOrComplementOperator negationComplementExpression | exponentiationExpression
     * }
     */
    void parseNegationComplementExpression(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * nonemptyCondition : (LPAREN NONEMPTY LIDENTIFIER RPAREN) => LPAREN NONEMPTY impliedVariable RPAREN | (LPAREN NONEMPTY compilerAnnotations (declarationStart|specificationStart)) => LPAREN NONEMPTY (specifiedVariable)? RPAREN | LPAREN NONEMPTY (expression)? RPAREN
     * }
     */
    void parseNonemptyCondition(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * nonstringLiteral : NATURAL_LITERAL | FLOAT_LITERAL | QUOTED_LITERAL | CHAR_LITERAL
     * }
     */
    void parseNonstringLiteral(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * notOperator : NOT_OP
     * }
     */
    void parseNotOperator(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * objectArgument : OBJECT_DEFINITION memberNameDeclaration (extendedType)? (satisfiedTypes)? (classBody | SEMICOLON)
     * }
     */
    void parseObjectArgument(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * objectDeclaration : OBJECT_DEFINITION memberNameDeclaration (extendedType)? (satisfiedTypes)? (classBody | SEMICOLON)
     * }
     */
    void parseObjectDeclaration(PsiBuilder builder) {
    }

    /**
     * {@code
     * packageName : LIDENTIFIER
     * }
     */
    void parsePackageName(PsiBuilder builder) {
        getToken(builder, CeylonToken.LIDENTIFIER, "packageName expected");
    }

    /**
     * TODO: Implement
     * {@code
     * parameter : parameterType memberName ((valueParameter)? | (parameters)+) (specifier)?
     * }
     */
    void parseParameter(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * parameterDeclaration : compilerAnnotations annotations parameter
     * }
     */
    void parseParameterDeclaration(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * parameters : LPAREN (parameterDeclaration (COMMA ((~(COMPILER_ANNOTATION | LIDENTIFIER | UIDENTIFIER)) => | parameterDeclaration))*)? RPAREN
     * }
     */
    void parseParameters(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * parametersStart : LPAREN (annotatedDeclarationStart | RPAREN)
     * }
     */
    void parseParametersStart(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * parameterType : type (ELLIPSIS)? | VOID_MODIFIER
     * }
     */
    void parseParameterType(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * parExpression : LPAREN assignmentExpression RPAREN
     * }
     */
    void parseParExpression(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * positionalArgument : (FUNCTION_MODIFIER|parametersStart)=> (FUNCTION_MODIFIER)? parameters ((parametersStart)=> parameters)* expression | VALUE_MODIFIER expression | expression
     * }
     */
    void parsePositionalArgument(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * positionalArguments : LPAREN (positionalArgument (COMMA (positionalArgument ))* (ELLIPSIS)?)? (comprehension)? RPAREN
     * }
     */
    void parsePositionalArguments(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * postfixIncrementDecrementExpression : primary (postfixOperator)*
     * }
     */
    void parsePostfixIncrementDecrementExpression(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * postfixOperator : DECREMENT_OP | INCREMENT_OP
     * }
     */
    void parsePostfixOperator(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * prefixOperator : DECREMENT_OP | INCREMENT_OP
     * }
     */
    void parsePrefixOperator(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * prefixOperatorStart : DIFFERENCE_OP | INCREMENT_OP | DECREMENT_OP | COMPLEMENT_OP
     * }
     */
    void parsePrefixOperatorStart(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * primary : base (qualifiedReference | indexExpression | arguments)*
     * }
     */
    void parsePrimary(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * qualifiedReference TypeArgumentList typeArgumentList, boolean isMember] : memberSelectionOperator (memberReference | typeReference | (~(LIDENTIFIER|UIDENTIFIER))=>)
     * }
     */
    void parseQualifiedReference(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * qualifiedType : typeNameWithArguments (MEMBER_OP typeNameWithArguments)*
     * }
     */
    void parseQualifiedType(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * rangeIntervalEntryExpression : additiveExpression (rangeIntervalEntryOperator additiveExpression)?
     * }
     */
    void parseRangeIntervalEntryExpression(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * rangeIntervalEntryOperator : RANGE_OP | ENTRY_OP
     * }
     */
    void parseRangeIntervalEntryOperator(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * resource : LPAREN ((COMPILER_ANNOTATION|declarationStart|specificationStart) => specifiedVariable | expression) RPAREN
     * }
     */
    void parseResource(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * returnDirective : RETURN (expression)?
     * }
     */
    void parseReturnDirective(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * satisfiedTypes : SATISFIES qualifiedType (INTERSECTION_OP (qualifiedType ))*
     * }
     */
    void parseSatisfiedTypes(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * satisfiesCaseCondition : SATISFIES qualifiedType
     * }
     */
    void parseSatisfiesCaseCondition(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * satisfiesCondition : LPAREN SATISFIES (qualifiedType qualifiedType)? RPAREN
     * }
     */
    void parseSatisfiesCondition(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * selfReference : THIS | SUPER | OUTER
     * }
     */
    void parseSelfReference(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * sequencedArgument : compilerAnnotations expressions
     * }
     */
    void parseSequencedArgument(PsiBuilder builder) {
    }

    /**
     * {@code
     * setterDeclaration : ASSIGN memberNameDeclaration (block | SEMICOLON)
     * }
     */
    boolean parseSetterDeclaration(PsiBuilder builder) {
        if (!ParserUtils.lookAhead(builder, CeylonToken.ASSIGN)) {
            return false;
        }

        PsiBuilder.Marker marker = builder.mark();

        getToken(builder, CeylonToken.ASSIGN);

        parseMemberName(builder);

        if (!parseBlock(builder) && !getToken(builder, CeylonToken.SEMICOLON)) {
            builder.error("block or SEMICOLON expected");
        }

        marker.done(CeylonAstNode.ATTRIBUTE_SETTER_DEFINITION);
        return true;
    }

    /**
     * TODO: Implement
     * {@code
     * specificationStart : LIDENTIFIER '='
     * }
     */
    void parseSpecificationStart(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * specifiedVariable : variable (specifier)?
     * }
     */
    void parseSpecifiedVariable(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * specifier : SPECIFY expression
     * }
     */
    void parseSpecifier(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * statement : directiveStatement | controlStatement | expressionOrSpecificationStatement
     * }
     */
    void parseStatement(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * stringExpression : (STRING_LITERAL interpolatedExpressionStart) => stringTemplate | stringLiteral
     * }
     */
    void parseStringExpression(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * stringLiteral : STRING_LITERAL
     * }
     */
    void parseStringLiteral(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * stringTemplate : stringLiteral ((interpolatedExpressionStart) => expression stringLiteral)+
     * }
     */
    void parseStringTemplate(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * switchCaseElse : switchHeader cases
     * }
     */
    void parseSwitchCaseElse(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * switchHeader : SWITCH_CLAUSE LPAREN expression RPAREN
     * }
     */
    void parseSwitchHeader(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * thenElseExpression : disjunctionExpression (thenElseOperator disjunctionExpression)*
     * }
     */
    void parseThenElseExpression(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * thenElseOperator : ELSE_CLAUSE | THEN_CLAUSE
     * }
     */
    void parseThenElseOperator(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * throwDirective : THROW (expression)?
     * }
     */
    void parseThrowDirective(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * tryBlock : TRY_CLAUSE (resource controlBlock | block)
     * }
     */
    void parseTryBlock(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * tryCatchFinally : tryBlock (catchBlock)* (finallyBlock)?
     * }
     */
    void parseTryCatchFinally(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * type : unionType
     * }
     */
    void parseType(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * typeAlias : typeNameDeclaration SPECIFY
     * }
     */
    void parseTypeAlias(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * typeArgument : type (ELLIPSIS)?
     * }
     */
    void parseTypeArgument(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * typeArguments : SMALLER_OP typeArgument (COMMA (typeArgument ))* LARGER_OP
     * }
     */
    void parseTypeArguments(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * typeArgumentsStart : SMALLER_OP (UIDENTIFIER ('.' UIDENTIFIER)* (DEFAULT_OP|ARRAY)* ((INTERSECTION_OP|UNION_OP|ENTRY_OP) UIDENTIFIER ('.' UIDENTIFIER)* (DEFAULT_OP|ARRAY)*)* (LARGER_OP|SMALLER_OP|COMMA|ELLIPSIS) | LARGER_OP)
     * }
     */
    void parseTypeArgumentsStart(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * typeConstraint : compilerAnnotations TYPE_CONSTRAINT typeNameDeclaration (parameters)? (caseTypes)? (satisfiedTypes)? (abstractedType)?
     * }
     */
    void parseTypeConstraint(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * typeConstraints : (typeConstraint)+
     * }
     */
    void parseTypeConstraints(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * typedMethodOrAttributeDeclaration : type memberNameDeclaration ((typeParameters)? (parameters)+ (typeConstraints)? (methodBody[$type.type] | (specifier)? SEMICOLON) | (specifier | initializer)? SEMICOLON | attributeBody[$type.type])
     * }
     */
    void parseTypedMethodOrAttributeDeclaration(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * typedMethodOrGetterArgument : type memberNameDeclaration ((parameters)+)? methodBody[$type.type]
     * }
     */
    void parseTypedMethodOrGetterArgument(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * typeName : UIDENTIFIER
     * }
     */
    void parseTypeName(PsiBuilder builder) {
        getToken(builder, CeylonToken.UIDENTIFIER, "UIDENTIFIER expected");
    }

    /**
     * TODO: Implement
     * {@code
     * typeNameDeclaration : typeName | memberName
     * }
     */
    void parseTypeNameDeclaration(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * typeNameWithArguments : typeName (typeArguments)?
     * }
     */
    void parseTypeNameWithArguments(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * typeOperator : IS_OP | EXTENDS | SATISFIES
     * }
     */
    void parseTypeOperator(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * typeParameter : (variance)? typeNameDeclaration | typeNameDeclaration ELLIPSIS
     * }
     */
    void parseTypeParameter(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * typeParameters : SMALLER_OP typeParameter (COMMA (typeParameter ))* LARGER_OP
     * }
     */
    void parseTypeParameters(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * typeReference : typeName ((typeArgumentsStart) => typeArguments)?
     * }
     */
    void parseTypeReference(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * typeSpecifier : SPECIFY qualifiedType
     * }
     */
    void parseTypeSpecifier(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * unaryMinusOrComplementOperator : DIFFERENCE_OP | SUM_OP | COMPLEMENT_OP
     * }
     */
    void parseUnaryMinusOrComplementOperator(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * unionType : intersectionType ((UNION_OP (intersectionType ))+)?
     * }
     */
    void parseUnionType(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * valueParameter : ENTRY_OP type memberName
     * }
     */
    void parseValueParameter(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * var : (type memberName (parameters)* | memberName | memberName (parameters)+)
     * }
     */
    void parseVar(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * variable : compilerAnnotations var
     * }
     */
    void parseVariable(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * variance : IN_OP | OUT
     * }
     */
    void parseVariance(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * voidOrInferredMethodArgument : (VOID_MODIFIER | FUNCTION_MODIFIER) memberNameDeclaration (parameters)* block
     * }
     */
    void parseVoidOrInferredMethodArgument(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * voidOrInferredMethodDeclaration : (VOID_MODIFIER | FUNCTION_MODIFIER) memberNameDeclaration (typeParameters)? (parameters)* (typeConstraints)? (block | (specifier)? SEMICOLON)
     * }
     */
    void parseVoidOrInferredMethodDeclaration(PsiBuilder builder) {
    }

    /**
     * TODO: Implement
     * {@code
     * whileBlock : WHILE_CLAUSE condition controlBlock
     * }
     */
    void parseWhileBlock(PsiBuilder builder) {
    }


    /**
     * TODO: Implement
     * {@code
     * whileLoop : whileBlock
     * }
     */
    void parseWhileLoop(PsiBuilder builder) {
    }

}
