package org.ceylon.idea.lang.parser;

import com.intellij.lang.ASTNode;
import com.intellij.lang.PsiBuilder;
import com.intellij.lang.PsiParser;
import com.intellij.psi.tree.IElementType;
import org.ceylon.idea.lang.lexer.CeylonToken;
import org.ceylon.idea.lang.parser.parsing.old.ImportStatementParser;
import org.jetbrains.annotations.NotNull;

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
     * {@code
     * abbreviatedType : qualifiedType (DEFAULT_OP | ARRAY)*
     * }
     */
    void parseAbbreviatedType(PsiBuilder builder) {
    }

    /**
     * {@code
     * abstractedType : ABSTRACTED_TYPE qualifiedType
     * }
     */
    void parseAbstractedType(PsiBuilder builder) {
    }

    /**
     * {@code
     * adaptedTypes : ADAPTED_TYPES qualifiedType (INTERSECTION_OP (qualifiedType ))*
     * }
     */
    void parseAdaptedTypes(PsiBuilder builder) {
    }

    /**
     * {@code
     * additiveExpression : multiplicativeExpression (additiveOperator multiplicativeExpression)*
     * }
     */
    void parseAdditiveExpression(PsiBuilder builder) {
    }

    /**
     * {@code
     * additiveOperator : SUM_OP | DIFFERENCE_OP | UNION_OP | XOR_OP | COMPLEMENT_OP
     * }
     */
    void parseAdditiveOperator(PsiBuilder builder) {
    }

    /**
     * {@code
     * annotatedDeclarationStart : annotation* declarationStart
     * }
     */
    void parseAnnotatedDeclarationStart(PsiBuilder builder) {
    }

    /**
     * {@code
     * annotation : annotationName annotationArguments
     * }
     */
    void parseAnnotation(PsiBuilder builder) {
    }

    /**
     * {@code
     * annotationArguments : arguments | literalArguments
     * }
     */
    void parseAnnotationArguments(PsiBuilder builder) {
    }

    /**
     * {@code
     * annotationName : LIDENTIFIER
     * }
     */
    void parseAnnotationName(PsiBuilder builder) {
    }

    /**
     * {@code
     * annotations : (annotation)*
     * }
     */
    void parseAnnotations(PsiBuilder builder) {
    }

    /**
     * {@code
     * arguments : positionalArguments | namedArguments
     * }
     */
    void parseArguments(PsiBuilder builder) {
    }

    /**
     * {@code
     * assignmentExpression : thenElseExpression (assignmentOperator assignmentExpression)?
     * }
     */
    void parseAssignmentExpression(PsiBuilder builder) {
    }

    /**
     * {@code
     * assignmentOperator : ASSIGN_OP | ADD_ASSIGN_OP | SUBTRACT_ASSIGN_OP | MULTIPLY_ASSIGN_OP | DIVIDE_ASSIGN_OP | REMAINDER_ASSIGN_OP | INTERSECT_ASSIGN_OP | UNION_ASSIGN_OP | XOR_ASSIGN_OP | COMPLEMENT_ASSIGN_OP | AND_ASSIGN_OP | OR_ASSIGN_OP
     * }
     */
    void parseAssignmentOperator(PsiBuilder builder) {
    }

    /**
     * {@code
     * attributeBody[StaticType type] : (namedArguments) => namedArguments | block
     * }
     */
    void parseAttributeBody(PsiBuilder builder) {
    }

    /**
     * {@code
     * base : nonstringLiteral | stringExpression | enumeration | selfReference | typeReference | memberReference | parExpression
     * }
     */
    void parseBase(PsiBuilder builder) {
    }

    /**
     * {@code
     * block : LBRACE (declarationOrStatement)* RBRACE
     * }
     */
    void parseBlock(PsiBuilder builder) {
    }

    /**
     * {@code
     * booleanCondition : LPAREN (expression)? RPAREN
     * }
     */
    void parseBooleanCondition(PsiBuilder builder) {
    }

    /**
     * {@code
     * breakDirective : BREAK
     * }
     */
    void parseBreakDirective(PsiBuilder builder) {
    }

    /**
     * {@code
     * caseBlock : CASE_CLAUSE LPAREN caseItem RPAREN block
     * }
     */
    void parseCaseBlock(PsiBuilder builder) {
    }

    /**
     * {@code
     * caseItem : (IS_OP)=>isCaseCondition | (SATISFIES)=>satisfiesCaseCondition | matchCaseCondition
     * }
     */
    void parseCaseItem(PsiBuilder builder) {
    }

    /**
     * {@code
     * cases : (caseBlock)+ (defaultCaseBlock)?
     * }
     */
    void parseCases(PsiBuilder builder) {
    }

    /**
     * {@code
     * caseType : qualifiedType | memberName
     * }
     */
    void parseCaseType(PsiBuilder builder) {
    }

    /**
     * {@code
     * caseTypes : CASE_TYPES caseType (UNION_OP (caseType ))*
     * }
     */
    void parseCaseTypes(PsiBuilder builder) {
    }

    /**
     * {@code
     * catchBlock : CATCH_CLAUSE catchVariable block
     * }
     */
    void parseCatchBlock(PsiBuilder builder) {
    }

    /**
     * {@code
     * catchVariable : LPAREN (variable)? RPAREN
     * }
     */
    void parseCatchVariable(PsiBuilder builder) {
    }

    /**
     * {@code
     * classBody : LBRACE (declarationOrStatement)* RBRACE
     * }
     */
    void parseClassBody(PsiBuilder builder) {
    }

    /**
     * {@code
     * classDeclaration : CLASS_DEFINITION typeNameDeclaration (typeParameters)? (parameters)? (caseTypes)? (extendedType)? (satisfiedTypes)? (typeConstraints)? (classBody | (typeSpecifier)? SEMICOLON)
     * }
     */
    void parseClassDeclaration(PsiBuilder builder) {
    }

    /**
     * {@code
     * comparisonExpression : existenceEmptinessExpression (comparisonOperator existenceEmptinessExpression | typeOperator qualifiedType)? | typeOperator qualifiedType existenceEmptinessExpression
     * }
     */
    void parseComparisonExpression(PsiBuilder builder) {
    }

    /**
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
            ParserUtils.getToken(builder, CeylonToken.SEMICOLON, "SEMICOLON expected");
        }

        parseImportList(builder);

        // TODO: declarations
    }

    /**
     * {@code
     * compilerAnnotation : COMPILER_ANNOTATION annotationName (INDEX_OP stringLiteral RBRACKET)?
     * }
     */
    boolean parseCompilerAnnotation(PsiBuilder builder) {
        PsiBuilder.Marker marker = builder.mark();
        if (!ParserUtils.getToken(builder, CeylonToken.COMPILER_ANNOTATION)) {
            marker.rollbackTo();
            return false;
        }
        ParserUtils.getToken(builder, CeylonToken.LIDENTIFIER, "LIDENTIFIER.expected");
        ParserUtils.getToken(builder, CeylonToken.STRING_LITERAL);

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
            ParserUtils.skipNLS(builder);
        }

        return hasCompilerAnnotations;
    }

    /**
     * {@code
     * comprehension : forComprehensionClause
     * }
     */
    void parseComprehension(PsiBuilder builder) {
    }

    /**
     * {@code
     * comprehensionClause : forComprehensionClause | ifComprehensionClause | expressionComprehensionClause
     * }
     */
    void parseComprehensionClause(PsiBuilder builder) {
    }

    /**
     * {@code
     * condition : (LPAREN EXISTS)=>existsCondition | (LPAREN NONEMPTY)=>nonemptyCondition | (LPAREN IS_OP)=>isCondition | (LPAREN SATISFIES)=>satisfiesCondition | booleanCondition
     * }
     */
    void parseCondition(PsiBuilder builder) {
    }

    /**
     * {@code
     * conjunctionExpression : logicalNegationExpression (conjunctionOperator logicalNegationExpression)*
     * }
     */
    void parseConjunctionExpression(PsiBuilder builder) {
    }

    /**
     * {@code
     * conjunctionOperator : AND_OP
     * }
     */
    void parseConjunctionOperator(PsiBuilder builder) {
    }

    /**
     * {@code
     * containment : IN_OP (expression)?
     * }
     */
    void parseContainment(PsiBuilder builder) {
    }

    /**
     * {@code
     * continueDirective : CONTINUE
     * }
     */
    void parseContinueDirective(PsiBuilder builder) {
    }

    /**
     * {@code
     * controlBlock : ((LBRACE)=> block )
     * }
     */
    void parseControlBlock(PsiBuilder builder) {
    }

    /**
     * {@code
     * controlStatement : ifElse | switchCaseElse | whileLoop | forElse | tryCatchFinally
     * }
     */
    void parseControlStatement(PsiBuilder builder) {
    }

    /**
     * {@code
     * declaration : annotations (objectDeclaration | setterDeclaration | voidOrInferredMethodDeclaration | inferredAttributeDeclaration | typedMethodOrAttributeDeclaration | classDeclaration | interfaceDeclaration)
     * }
     */
    void parseDeclaration(PsiBuilder builder) {
    }

    /**
     * {@code
     * declarationKeyword : VALUE_MODIFIER | FUNCTION_MODIFIER | ASSIGN | VOID_MODIFIER | INTERFACE_DEFINITION | CLASS_DEFINITION | OBJECT_DEFINITION
     * }
     */
    void parseDeclarationKeyword(PsiBuilder builder) {
    }

    /**
     * {@code
     * declarationOrStatement : compilerAnnotations ((annotatedDeclarationStart) => declaration | statement)
     * }
     */
    void parseDeclarationOrStatement(PsiBuilder builder) {
    }

    /**
     * {@code
     * declarationStart : declarationKeyword | type (ELLIPSIS | LIDENTIFIER)
     * }
     */
    void parseDeclarationStart(PsiBuilder builder) {
    }

    /**
     * {@code
     * defaultCaseBlock : ELSE_CLAUSE block
     * }
     */
    void parseDefaultCaseBlock(PsiBuilder builder) {
    }

    /**
     * {@code
     * defaultExpression : negationComplementExpression (defaultOperator negationComplementExpression)*
     * }
     */
    void parseDefaultExpression(PsiBuilder builder) {
    }

    /**
     * {@code
     * defaultOperator : DEFAULT_OP
     * }
     */
    void parseDefaultOperator(PsiBuilder builder) {
    }

    /**
     * {@code
     * directive : returnDirective | throwDirective | breakDirective | continueDirective
     * }
     */
    void parseDirective(PsiBuilder builder) {
    }

    /**
     * {@code
     * directiveStatement : directive SEMICOLON
     * }
     */
    void parseDirectiveStatement(PsiBuilder builder) {
    }

    /**
     * {@code
     * disjunctionExpression : conjunctionExpression (disjunctionOperator conjunctionExpression)*
     * }
     */
    void parseDisjunctionExpression(PsiBuilder builder) {
    }

    /**
     * {@code
     * disjunctionOperator : OR_OP
     * }
     */
    void parseDisjunctionOperator(PsiBuilder builder) {
    }

    /**
     * {@code
     * elementSelectionOperator : SAFE_INDEX_OP | INDEX_OP
     * }
     */
    void parseElementSelectionOperator(PsiBuilder builder) {
    }

    /**
     * {@code
     * elseBlock : ELSE_CLAUSE (elseIf | block)
     * }
     */
    void parseElseBlock(PsiBuilder builder) {
    }

    /**
     * {@code
     * elseIf : ifElse
     * }
     */
    void parseElseIf(PsiBuilder builder) {
    }

    /**
     * {@code
     * entryType : abbreviatedType (ENTRY_OP (abbreviatedType ))?
     * }
     */
    void parseEntryType(PsiBuilder builder) {
    }

    /**
     * {@code
     * enumeration : LBRACE (expressions | comprehension)? RBRACE
     * }
     */
    void parseEnumeration(PsiBuilder builder) {
    }

    /**
     * {@code
     * equalityExpression : comparisonExpression (equalityOperator comparisonExpression)?
     * }
     */
    void parseEqualityExpression(PsiBuilder builder) {
    }

    /**
     * {@code
     * equalityOperator : EQUAL_OP | NOT_EQUAL_OP | IDENTICAL_OP
     * }
     */
    void parseEqualityOperator(PsiBuilder builder) {
    }

    /**
     * {@code
     * erasure : LPAREN (typeName (COMMA typeName)*)? RPAREN
     * }
     */
    void parseErasure(PsiBuilder builder) {
    }

    /**
     * {@code
     * existenceEmptinessExpression : rangeIntervalEntryExpression (existsNonemptyOperator)? | existsNonemptyOperator rangeIntervalEntryExpression
     * }
     */
    void parseExistenceEmptinessExpression(PsiBuilder builder) {
    }

    /**
     * {@code
     * existsCondition : (LPAREN EXISTS LIDENTIFIER RPAREN) => LPAREN EXISTS impliedVariable RPAREN | (LPAREN EXISTS compilerAnnotations (declarationStart|specificationStart)) => LPAREN EXISTS specifiedVariable RPAREN | LPAREN EXISTS (expression)? RPAREN
     * }
     */
    void parseExistsCondition(PsiBuilder builder) {
    }

    /**
     * {@code
     * existsNonemptyOperator : EXISTS | NONEMPTY
     * }
     */
    void parseExistsNonemptyOperator(PsiBuilder builder) {
    }

    /**
     * {@code
     * exponentiationExpression : incrementDecrementExpression (exponentiationOperator exponentiationExpression)?
     * }
     */
    void parseExponentiationExpression(PsiBuilder builder) {
    }

    /**
     * {@code
     * exponentiationOperator : POWER_OP
     * }
     */
    void parseExponentiationOperator(PsiBuilder builder) {
    }

    /**
     * {@code
     * expression : assignmentExpression
     * }
     */
    void parseExpression(PsiBuilder builder) {
    }

    /**
     * {@code
     * expressionComprehensionClause : expression |
     * }
     */
    void parseExpressionComprehensionClause(PsiBuilder builder) {
    }

    /**
     * {@code
     * expressionOrSpecificationStatement : expression (specifier)? (SEMICOLON | COMMA)
     * }
     */
    void parseExpressionOrSpecificationStatement(PsiBuilder builder) {
    }

    /**
     * {@code
     * expressions : expression (COMMA (expression ))*
     * }
     */
    void parseExpressions(PsiBuilder builder) {
    }

    /**
     * {@code
     * extendedType : EXTENDS (qualifiedType | SUPER MEMBER_OP typeReference) (positionalArguments )
     * }
     */
    void parseExtendedType(PsiBuilder builder) {
    }

    /**
     * {@code
     * failBlock : ELSE_CLAUSE block
     * }
     */
    void parseFailBlock(PsiBuilder builder) {
    }

    /**
     * {@code
     * finallyBlock : FINALLY_CLAUSE block
     * }
     */
    void parseFinallyBlock(PsiBuilder builder) {
    }

    /**
     * {@code
     * forBlock : FOR_CLAUSE forIterator controlBlock
     * }
     */
    void parseForBlock(PsiBuilder builder) {
    }

    /**
     * {@code
     * forComprehensionClause : FOR_CLAUSE forIterator comprehensionClause
     * }
     */
    void parseForComprehensionClause(PsiBuilder builder) {
    }

    /**
     * {@code
     * forElse : forBlock (failBlock)?
     * }
     */
    void parseForElse(PsiBuilder builder) {
    }

    /**
     * {@code
     * forIterator : LPAREN compilerAnnotations (var (containment | ENTRY_OP var containment)?)? RPAREN
     * }
     */
    void parseForIterator(PsiBuilder builder) {
    }

    /**
     * {@code
     * ifBlock : IF_CLAUSE condition controlBlock
     * }
     */
    void parseIfBlock(PsiBuilder builder) {
    }

    /**
     * {@code
     * ifComprehensionClause : IF_CLAUSE condition comprehensionClause
     * }
     */
    void parseIfComprehensionClause(PsiBuilder builder) {
    }

    /**
     * {@code
     * ifElse : ifBlock (elseBlock)?
     * }
     */
    void parseIfElse(PsiBuilder builder) {
    }

    /**
     * {@code
     * impliedVariable : memberName
     * }
     */
    void parseImpliedVariable(PsiBuilder builder) {
    }

    /**
     * {@code
     * importDeclaration : IMPORT (packageName (MEMBER_OP (packageName))*) importElementList
     * }
     */
    void parseImportDeclaration(PsiBuilder builder) {
    }

    /**
     * {@code
     * importElement : compilerAnnotations (memberAlias? memberName (erasure)? | typeAlias? typeName (erasure)? (importElementList)?)
     * }
     */
    void parseImportElement(PsiBuilder builder) {
    }

    /**
     * {@code
     * importElementList : LBRACE (importElement (COMMA importElement)* (COMMA (importWildcard ))? | importWildcard)? RBRACE
     * }
     */
    void parseImportElementList(PsiBuilder builder) {
    }

    /**
     * {@code
     * importList : (importDeclaration)*
     * }
     */
    void parseImportList(PsiBuilder builder) {
        PsiBuilder.Marker marker = builder.mark();

        if (!ParserUtils.lookAhead(builder, CeylonToken.IMPORT)) {
            marker.rollbackTo(); // TODO: do we really need to mark and rollback here?
            return;
        }

        while (ImportStatementParser.parse(builder)) {
            ParserUtils.skipNLS(builder);
        }

        marker.done(CeylonAstNode.IMPORT_LIST);
    }

    /**
     * {@code
     * importWildcard : ELLIPSIS
     * }
     */
    void parseImportWildcard(PsiBuilder builder) {
    }

    /**
     * {@code
     * incrementDecrementExpression : prefixOperator incrementDecrementExpression | postfixIncrementDecrementExpression
     * }
     */
    void parseIncrementDecrementExpression(PsiBuilder builder) {
    }

    /**
     * {@code
     * index : additiveExpression
     * }
     */
    void parseIndex(PsiBuilder builder) {
    }

    /**
     * {@code
     * indexExpression : elementSelectionOperator indexOrIndexRange RBRACKET
     * }
     */
    void parseIndexExpression(PsiBuilder builder) {
    }

    /**
     * {@code
     * indexOrIndexRange : index (| '...' | '..' index)
     * }
     */
    void parseIndexOrIndexRange(PsiBuilder builder) {
    }

    /**
     * {@code
     * inferredAttributeDeclaration : VALUE_MODIFIER memberNameDeclaration ((specifier | initializer)? SEMICOLON | block)
     * }
     */
    void parseInferredAttributeDeclaration(PsiBuilder builder) {
    }

    /**
     * {@code
     * inferredGetterArgument : VALUE_MODIFIER memberNameDeclaration block
     * }
     */
    void parseInferredGetterArgument(PsiBuilder builder) {
    }

    /**
     * {@code
     * initializer : ASSIGN_OP expression
     * }
     */
    void parseInitializer(PsiBuilder builder) {
    }

    /**
     * {@code
     * inlineFunctionalArgument : memberName ((parametersStart) => parameters)? (LPAREN expression RPAREN | block)
     * }
     */
    void parseInlineFunctionalArgument(PsiBuilder builder) {
    }

    /**
     * {@code
     * interfaceBody : LBRACE (declarationOrStatement)* RBRACE
     * }
     */
    void parseInterfaceBody(PsiBuilder builder) {
    }

    /**
     * {@code
     * interfaceDeclaration : INTERFACE_DEFINITION typeNameDeclaration (typeParameters)? (caseTypes)? (adaptedTypes)? (satisfiedTypes)? (typeConstraints)? (interfaceBody | (typeSpecifier)? SEMICOLON)
     * }
     */
    void parseInterfaceDeclaration(PsiBuilder builder) {
    }

    /**
     * {@code
     * interpolatedExpressionStart : LPAREN | LBRACE | LIDENTIFIER | UIDENTIFIER | selfReference | nonstringLiteral | prefixOperatorStart
     * }
     */
    void parseInterpolatedExpressionStart(PsiBuilder builder) {
    }

    /**
     * {@code
     * intersectionType : entryType ((INTERSECTION_OP (entryType ))+)?
     * }
     */
    void parseIntersectionType(PsiBuilder builder) {
    }

    /**
     * {@code
     * isCaseCondition : IS_OP type
     * }
     */
    void parseIsCaseCondition(PsiBuilder builder) {
    }

    /**
     * {@code
     * isCondition : (LPAREN IS_OP type LIDENTIFIER RPAREN) => LPAREN IS_OP type impliedVariable RPAREN | (LPAREN IS_OP type LIDENTIFIER SPECIFY) => LPAREN IS_OP (type memberName specifier)? RPAREN | LPAREN IS_OP abbreviatedType (expression)? RPAREN
     * }
     */
    void parseIsCondition(PsiBuilder builder) {
    }

    /**
     * {@code
     * literalArgument : nonstringLiteral | stringLiteral
     * }
     */
    void parseLiteralArgument(PsiBuilder builder) {
    }

    /**
     * {@code
     * literalArguments : (literalArgument)*
     * }
     */
    void parseLiteralArguments(PsiBuilder builder) {
    }

    /**
     * {@code
     * logicalNegationExpression : notOperator logicalNegationExpression | equalityExpression
     * }
     */
    void parseLogicalNegationExpression(PsiBuilder builder) {
    }

    /**
     * {@code
     * matchCaseCondition : expressions
     * }
     */
    void parseMatchCaseCondition(PsiBuilder builder) {
    }

    /**
     * {@code
     * memberAlias : memberNameDeclaration SPECIFY
     * }
     */
    void parseMemberAlias(PsiBuilder builder) {
    }

    /**
     * {@code
     * memberName : LIDENTIFIER
     * }
     */
    void parseMemberName(PsiBuilder builder) {
    }

    /**
     * {@code
     * memberNameDeclaration : memberName | typeName
     * }
     */
    void parseMemberNameDeclaration(PsiBuilder builder) {
    }

    /**
     * {@code
     * memberReference : memberName ((typeArgumentsStart) => typeArguments)?
     * }
     */
    void parseMemberReference(PsiBuilder builder) {
    }

    /**
     * {@code
     * memberSelectionOperator : MEMBER_OP | SAFE_MEMBER_OP | SPREAD_OP
     * }
     */
    void parseMemberSelectionOperator(PsiBuilder builder) {
    }

    /**
     * {@code
     * methodBody[StaticType type] : (namedArguments) => namedArguments | block
     * }
     */
    void parseMethodBody(PsiBuilder builder) {
    }

    /**
     * {@code
     * multiplicativeExpression : defaultExpression (multiplicativeOperator defaultExpression)*
     * }
     */
    void parseMultiplicativeExpression(PsiBuilder builder) {
    }

    /**
     * {@code
     * multiplicativeOperator : PRODUCT_OP | QUOTIENT_OP | REMAINDER_OP | INTERSECTION_OP
     * }
     */
    void parseMultiplicativeOperator(PsiBuilder builder) {
    }

    /**
     * {@code
     * namedArgument : compilerAnnotations (namedSpecifiedArgument | namedArgumentDeclaration)
     * }
     */
    void parseNamedArgument(PsiBuilder builder) {
    }

    /**
     * {@code
     * namedArgumentDeclaration : objectArgument | typedMethodOrGetterArgument | voidOrInferredMethodArgument | inferredGetterArgument
     * }
     */
    void parseNamedArgumentDeclaration(PsiBuilder builder) {
    }

    /**
     * {@code
     * namedArguments : LBRACE ((namedArgumentStart) => namedArgument)* (sequencedArgument | comprehension)? RBRACE
     * }
     */
    void parseNamedArguments(PsiBuilder builder) {
    }

    /**
     * {@code
     * namedArgumentStart : compilerAnnotation* (specificationStart | declarationStart)
     * }
     */
    void parseNamedArgumentStart(PsiBuilder builder) {
    }

    /**
     * {@code
     * namedSpecifiedArgument : memberNameDeclaration specifier SEMICOLON
     * }
     */
    void parseNamedSpecifiedArgument(PsiBuilder builder) {
    }

    /**
     * {@code
     * negationComplementExpression : unaryMinusOrComplementOperator negationComplementExpression | exponentiationExpression
     * }
     */
    void parseNegationComplementExpression(PsiBuilder builder) {
    }

    /**
     * {@code
     * nonemptyCondition : (LPAREN NONEMPTY LIDENTIFIER RPAREN) => LPAREN NONEMPTY impliedVariable RPAREN | (LPAREN NONEMPTY compilerAnnotations (declarationStart|specificationStart)) => LPAREN NONEMPTY (specifiedVariable)? RPAREN | LPAREN NONEMPTY (expression)? RPAREN
     * }
     */
    void parseNonemptyCondition(PsiBuilder builder) {
    }

    /**
     * {@code
     * nonstringLiteral : NATURAL_LITERAL | FLOAT_LITERAL | QUOTED_LITERAL | CHAR_LITERAL
     * }
     */
    void parseNonstringLiteral(PsiBuilder builder) {
    }

    /**
     * {@code
     * notOperator : NOT_OP
     * }
     */
    void parseNotOperator(PsiBuilder builder) {
    }

    /**
     * {@code
     * objectArgument : OBJECT_DEFINITION memberNameDeclaration (extendedType)? (satisfiedTypes)? (classBody | SEMICOLON)
     * }
     */
    void parseObjectArgument(PsiBuilder builder) {
    }

    /**
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
    }

    /**
     * {@code
     * parameter : parameterType memberName ((valueParameter)? | (parameters)+) (specifier)?
     * }
     */
    void parseParameter(PsiBuilder builder) {
    }

    /**
     * {@code
     * parameterDeclaration : compilerAnnotations annotations parameter
     * }
     */
    void parseParameterDeclaration(PsiBuilder builder) {
    }

    /**
     * {@code
     * parameters : LPAREN (parameterDeclaration (COMMA ((~(COMPILER_ANNOTATION | LIDENTIFIER | UIDENTIFIER)) => | parameterDeclaration))*)? RPAREN
     * }
     */
    void parseParameters(PsiBuilder builder) {
    }

    /**
     * {@code
     * parametersStart : LPAREN (annotatedDeclarationStart | RPAREN)
     * }
     */
    void parseParametersStart(PsiBuilder builder) {
    }

    /**
     * {@code
     * parameterType : type (ELLIPSIS)? | VOID_MODIFIER
     * }
     */
    void parseParameterType(PsiBuilder builder) {
    }

    /**
     * {@code
     * parExpression : LPAREN assignmentExpression RPAREN
     * }
     */
    void parseParExpression(PsiBuilder builder) {
    }

    /**
     * {@code
     * positionalArgument : (FUNCTION_MODIFIER|parametersStart)=> (FUNCTION_MODIFIER)? parameters ((parametersStart)=> parameters)* expression | VALUE_MODIFIER expression | expression
     * }
     */
    void parsePositionalArgument(PsiBuilder builder) {
    }

    /**
     * {@code
     * positionalArguments : LPAREN (positionalArgument (COMMA (positionalArgument ))* (ELLIPSIS)?)? (comprehension)? RPAREN
     * }
     */
    void parsePositionalArguments(PsiBuilder builder) {
    }

    /**
     * {@code
     * postfixIncrementDecrementExpression : primary (postfixOperator)*
     * }
     */
    void parsePostfixIncrementDecrementExpression(PsiBuilder builder) {
    }

    /**
     * {@code
     * postfixOperator : DECREMENT_OP | INCREMENT_OP
     * }
     */
    void parsePostfixOperator(PsiBuilder builder) {
    }

    /**
     * {@code
     * prefixOperator : DECREMENT_OP | INCREMENT_OP
     * }
     */
    void parsePrefixOperator(PsiBuilder builder) {
    }

    /**
     * {@code
     * prefixOperatorStart : DIFFERENCE_OP | INCREMENT_OP | DECREMENT_OP | COMPLEMENT_OP
     * }
     */
    void parsePrefixOperatorStart(PsiBuilder builder) {
    }

    /**
     * {@code
     * primary : base (qualifiedReference | indexExpression | arguments)*
     * }
     */
    void parsePrimary(PsiBuilder builder) {
    }

    /**
     * {@code
     * qualifiedReference TypeArgumentList typeArgumentList, boolean isMember] : memberSelectionOperator (memberReference | typeReference | (~(LIDENTIFIER|UIDENTIFIER))=>)
     * }
     */
    void parseQualifiedReference(PsiBuilder builder) {
    }

    /**
     * {@code
     * qualifiedType : typeNameWithArguments (MEMBER_OP typeNameWithArguments)*
     * }
     */
    void parseQualifiedType(PsiBuilder builder) {
    }

    /**
     * {@code
     * rangeIntervalEntryExpression : additiveExpression (rangeIntervalEntryOperator additiveExpression)?
     * }
     */
    void parseRangeIntervalEntryExpression(PsiBuilder builder) {
    }

    /**
     * {@code
     * rangeIntervalEntryOperator : RANGE_OP | ENTRY_OP
     * }
     */
    void parseRangeIntervalEntryOperator(PsiBuilder builder) {
    }

    /**
     * {@code
     * resource : LPAREN ((COMPILER_ANNOTATION|declarationStart|specificationStart) => specifiedVariable | expression) RPAREN
     * }
     */
    void parseResource(PsiBuilder builder) {
    }

    /**
     * {@code
     * returnDirective : RETURN (expression)?
     * }
     */
    void parseReturnDirective(PsiBuilder builder) {
    }

    /**
     * {@code
     * satisfiedTypes : SATISFIES qualifiedType (INTERSECTION_OP (qualifiedType ))*
     * }
     */
    void parseSatisfiedTypes(PsiBuilder builder) {
    }

    /**
     * {@code
     * satisfiesCaseCondition : SATISFIES qualifiedType
     * }
     */
    void parseSatisfiesCaseCondition(PsiBuilder builder) {
    }

    /**
     * {@code
     * satisfiesCondition : LPAREN SATISFIES (qualifiedType qualifiedType)? RPAREN
     * }
     */
    void parseSatisfiesCondition(PsiBuilder builder) {
    }

    /**
     * {@code
     * selfReference : THIS | SUPER | OUTER
     * }
     */
    void parseSelfReference(PsiBuilder builder) {
    }

    /**
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
    void parseSetterDeclaration(PsiBuilder builder) {
    }

    /**
     * {@code
     * specificationStart : LIDENTIFIER '='
     * }
     */
    void parseSpecificationStart(PsiBuilder builder) {
    }

    /**
     * {@code
     * specifiedVariable : variable (specifier)?
     * }
     */
    void parseSpecifiedVariable(PsiBuilder builder) {
    }

    /**
     * {@code
     * specifier : SPECIFY expression
     * }
     */
    void parseSpecifier(PsiBuilder builder) {
    }

    /**
     * {@code
     * statement : directiveStatement | controlStatement | expressionOrSpecificationStatement
     * }
     */
    void parseStatement(PsiBuilder builder) {
    }

    /**
     * {@code
     * stringExpression : (STRING_LITERAL interpolatedExpressionStart) => stringTemplate | stringLiteral
     * }
     */
    void parseStringExpression(PsiBuilder builder) {
    }

    /**
     * {@code
     * stringLiteral : STRING_LITERAL
     * }
     */
    void parseStringLiteral(PsiBuilder builder) {
    }

    /**
     * {@code
     * stringTemplate : stringLiteral ((interpolatedExpressionStart) => expression stringLiteral)+
     * }
     */
    void parseStringTemplate(PsiBuilder builder) {
    }

    /**
     * {@code
     * switchCaseElse : switchHeader cases
     * }
     */
    void parseSwitchCaseElse(PsiBuilder builder) {
    }

    /**
     * {@code
     * switchHeader : SWITCH_CLAUSE LPAREN expression RPAREN
     * }
     */
    void parseSwitchHeader(PsiBuilder builder) {
    }

    /**
     * {@code
     * thenElseExpression : disjunctionExpression (thenElseOperator disjunctionExpression)*
     * }
     */
    void parseThenElseExpression(PsiBuilder builder) {
    }

    /**
     * {@code
     * thenElseOperator : ELSE_CLAUSE | THEN_CLAUSE
     * }
     */
    void parseThenElseOperator(PsiBuilder builder) {
    }

    /**
     * {@code
     * throwDirective : THROW (expression)?
     * }
     */
    void parseThrowDirective(PsiBuilder builder) {
    }

    /**
     * {@code
     * tryBlock : TRY_CLAUSE (resource controlBlock | block)
     * }
     */
    void parseTryBlock(PsiBuilder builder) {
    }

    /**
     * {@code
     * tryCatchFinally : tryBlock (catchBlock)* (finallyBlock)?
     * }
     */
    void parseTryCatchFinally(PsiBuilder builder) {
    }

    /**
     * {@code
     * type : unionType
     * }
     */
    void parseType(PsiBuilder builder) {
    }

    /**
     * {@code
     * typeAlias : typeNameDeclaration SPECIFY
     * }
     */
    void parseTypeAlias(PsiBuilder builder) {
    }

    /**
     * {@code
     * typeArgument : type (ELLIPSIS)?
     * }
     */
    void parseTypeArgument(PsiBuilder builder) {
    }

    /**
     * {@code
     * typeArguments : SMALLER_OP typeArgument (COMMA (typeArgument ))* LARGER_OP
     * }
     */
    void parseTypeArguments(PsiBuilder builder) {
    }

    /**
     * {@code
     * typeArgumentsStart : SMALLER_OP (UIDENTIFIER ('.' UIDENTIFIER)* (DEFAULT_OP|ARRAY)* ((INTERSECTION_OP|UNION_OP|ENTRY_OP) UIDENTIFIER ('.' UIDENTIFIER)* (DEFAULT_OP|ARRAY)*)* (LARGER_OP|SMALLER_OP|COMMA|ELLIPSIS) | LARGER_OP)
     * }
     */
    void parseTypeArgumentsStart(PsiBuilder builder) {
    }

    /**
     * {@code
     * typeConstraint : compilerAnnotations TYPE_CONSTRAINT typeNameDeclaration (parameters)? (caseTypes)? (satisfiedTypes)? (abstractedType)?
     * }
     */
    void parseTypeConstraint(PsiBuilder builder) {
    }

    /**
     * {@code
     * typeConstraints : (typeConstraint)+
     * }
     */
    void parseTypeConstraints(PsiBuilder builder) {
    }

    /**
     * {@code
     * typedMethodOrAttributeDeclaration : type memberNameDeclaration ((typeParameters)? (parameters)+ (typeConstraints)? (methodBody[$type.type] | (specifier)? SEMICOLON) | (specifier | initializer)? SEMICOLON | attributeBody[$type.type])
     * }
     */
    void parseTypedMethodOrAttributeDeclaration(PsiBuilder builder) {
    }

    /**
     * {@code
     * typedMethodOrGetterArgument : type memberNameDeclaration ((parameters)+)? methodBody[$type.type]
     * }
     */
    void parseTypedMethodOrGetterArgument(PsiBuilder builder) {
    }

    /**
     * {@code
     * typeName : UIDENTIFIER
     * }
     */
    void parseTypeName(PsiBuilder builder) {
    }

    /**
     * {@code
     * typeNameDeclaration : typeName | memberName
     * }
     */
    void parseTypeNameDeclaration(PsiBuilder builder) {
    }

    /**
     * {@code
     * typeNameWithArguments : typeName (typeArguments)?
     * }
     */
    void parseTypeNameWithArguments(PsiBuilder builder) {
    }

    /**
     * {@code
     * typeOperator : IS_OP | EXTENDS | SATISFIES
     * }
     */
    void parseTypeOperator(PsiBuilder builder) {
    }

    /**
     * {@code
     * typeParameter : (variance)? typeNameDeclaration | typeNameDeclaration ELLIPSIS
     * }
     */
    void parseTypeParameter(PsiBuilder builder) {
    }

    /**
     * {@code
     * typeParameters : SMALLER_OP typeParameter (COMMA (typeParameter ))* LARGER_OP
     * }
     */
    void parseTypeParameters(PsiBuilder builder) {
    }

    /**
     * {@code
     * typeReference : typeName ((typeArgumentsStart) => typeArguments)?
     * }
     */
    void parseTypeReference(PsiBuilder builder) {
    }

    /**
     * {@code
     * typeSpecifier : SPECIFY qualifiedType
     * }
     */
    void parseTypeSpecifier(PsiBuilder builder) {
    }

    /**
     * {@code
     * unaryMinusOrComplementOperator : DIFFERENCE_OP | SUM_OP | COMPLEMENT_OP
     * }
     */
    void parseUnaryMinusOrComplementOperator(PsiBuilder builder) {
    }

    /**
     * {@code
     * unionType : intersectionType ((UNION_OP (intersectionType ))+)?
     * }
     */
    void parseUnionType(PsiBuilder builder) {
    }

    /**
     * {@code
     * valueParameter : ENTRY_OP type memberName
     * }
     */
    void parseValueParameter(PsiBuilder builder) {
    }

    /**
     * {@code
     * var : (type memberName (parameters)* | memberName | memberName (parameters)+)
     * }
     */
    void parseVar(PsiBuilder builder) {
    }

    /**
     * {@code
     * variable : compilerAnnotations var
     * }
     */
    void parseVariable(PsiBuilder builder) {
    }

    /**
     * {@code
     * variance : IN_OP | OUT
     * }
     */
    void parseVariance(PsiBuilder builder) {
    }

    /**
     * {@code
     * voidOrInferredMethodArgument : (VOID_MODIFIER | FUNCTION_MODIFIER) memberNameDeclaration (parameters)* block
     * }
     */
    void parseVoidOrInferredMethodArgument(PsiBuilder builder) {
    }

    /**
     * {@code
     * voidOrInferredMethodDeclaration : (VOID_MODIFIER | FUNCTION_MODIFIER) memberNameDeclaration (typeParameters)? (parameters)* (typeConstraints)? (block | (specifier)? SEMICOLON)
     * }
     */
    void parseVoidOrInferredMethodDeclaration(PsiBuilder builder) {
    }

    /**
     * {@code
     * whileBlock : WHILE_CLAUSE condition controlBlock
     * }
     */
    void parseWhileBlock(PsiBuilder builder) {
    }


    /**
     * {@code
     * whileLoop : whileBlock
     * }
     */
    void parseWhileLoop(PsiBuilder builder) {
    }

}
