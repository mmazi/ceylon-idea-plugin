package org.ceylon.idea.lang.parser;

import com.intellij.lang.PsiBuilder;
import com.intellij.psi.tree.IElementType;

import static org.ceylon.idea.lang.lexer.CeylonToken.*;

/*
<pre>

AbbreviatedType: Type Abbreviation*
Abbreviation: "?" | "[]"
AbstractedType: "abstracts" Type
AdaptedTypes: "adapts" Type ("&" Type)*
Annotation: MemberName ( Arguments | Literal+ )?
Arguments: PositionalArguments FunctionalArguments? | NamedArguments
Assignment: ":=" | ".=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^="| "~=" | "&&=" | "||=" ;
Atom: Literal | StringTemplate | SelfReference | ParExpression
Attribute: Annotation* (SimpleAttribute | AttributeGetter | AttributeSetter)
AttributeGetter: AttributeHeader Block
AttributeHeader: (UnionType | "value") MemberName
AttributeMeta: Type "." MemberName
AttributeSetter: "assign" MemberName Block
Block: "{" (Declaration | Statement)* "}"
BooleanCondition: Expression
Break: "break"
CallableParam: (UnionType | "void") MemberName Params+
CallableReference: MethodReference | InitializerReference
CallableVariable: (UnionType | "void")? MemberName Params+
Case: Expression ("," Expression)* | "is" UnionType | "satisfies" Type
CaseItem: "case" "(" Case ")" Block
Cases: CaseItem+ DefaultCaseItem?
CaseType: MemberName | Type
CaseTypes: "of" CaseType ("|" CaseType)*
Catch: "catch" "(" Variable ")" Block
CharacterLiteral: "`" Character "`"
Character: ~("`" | "\" | Tab | Formfeed | Newline | Return | Backspace) | EscapeSequence
Class: Annotation* ClassHeader (ClassBody | TypeSpecifier ";")
ClassBody: "{" (Declaration | Statement)* "}"
ClassHeader: "class" TypeName TypeParams? Params ClassInheritance TypeConstraints?
ClassInheritance: CaseTypes? Metatypes? ExtendedType? SatisfiedTypes?
ConcreteType: "this" "is"
ConditionalTypes: SatisfiedTypes Conditions
Condition: BooleanCondition | IsCondition | ExistsOrNonemptyCondition | SatisfiesCondition
Conditions: "if" "(" Condition ("&&" Condition)* ")"
Continue: "continue"
ControlStructure: IfElse | SwitchCaseElse | While | ForFail | TryCatchFinally
DateLiteral:  "'"  Digit{1,2} "/" Digit{1,2} "/" Digit{4}  "'"
Declaration: Method | Attribute | TypeDeclaration
DefaultCaseItem: "else" Block
DefaultParam: Param Specifier
Digit: "0".."9"
Digits: Digit+ | Digit{1..3} ("_" Digit{3})+
DimensionAtom: DimensionConstant | DimensionVariable | ParenDimension
DimensionConstant: "#" IntegerLiteral
Dimension: DimensionTerm ("+" DimensionTerm)*
DimensionTerm: (DimensionConstant "*")* DimensionAtom
DimensionVariable: TypeName | "#" MemberName
Directive: Return | Throw | Break | Continue
DirectiveStatement: Directive ";"
Else: "else" (Block | IfElse)
EntryParamPair: SimpleParam "->" SimpleParam
EntryType: AbbreviatedType ("->" AbbreviatedType)?
EntryVariablePair: Variable "->" Variable
EscapeSequence: "\" ("b" | "t" | "n" | "f" | "r" | "\" | "\"" | "'" | "`" )
ExistsOrNonemptyCondition: ("exists" | "nonempty") (Variable Specifier | MemberName)
Exponent: ("E"|"e") ("+"|"-")? Digits
Expression: Primary | OperatorExpression
ExpressionStatement: ( Assignment | IncrementOrDecrement | Invocation ) ";"
ExtendedType: "extends" ("super" ".")? Type PositionalArguments
Fail: "else" Block
Finally: "finally" Block
FloatLiteral: Digits ("." FractionalDigits (Exponent | Magnitude | FractionalMagnitude)? | FractionalMagnitude)
ForFail: For Fail?
For: "for" "(" ForIterator ")" Block
ForIterator: IteratorVariable "in" Expression
FractionalDigits: Digit+ | (Digit{3} "_")+ Digit{1..3}
FractionalMagnitude: "m" | "u" | "n" | "p" | "f"
FullPackageName: PackageName ("." PackageName)*
FunctionalArguments: (MemberName FunctionalBody)+
FunctionalBody: Params? ( Block | "(" Expression ")" )
FunctionalNamedArgument: (UnionType | "function" | "void") MemberName Params+ (Block | NamedArguments)
FunctionMeta: MemberName TypeArguments?
IdentifierChar: LowercaseChar | UppercaseChar | Digit
IfElse: If Else?
If: "if" "(" Condition ")" Block
ImportElement: ImportTypeElement | ImportMethodAttributeElement
ImportElements: ImportElement ("," ImportElement)* ("," ImportWildcard)? | ImportWildcard
Import: "import" FullPackageName "{" ImportElements? "}"
ImportMethodAttributeElement: MethodAttributeAlias? MemberName
ImportTypeElement: TypeAlias? TypeName
ImportWildcard: "..."
IncrementOrDecrement: "--" | "++" ;
Initializer: ":=" Expression
InitializerReference: (Receiver ".")? TypeName TypeArguments?
IntegerLiteral: Digits Magnitude?
Interface: Annotation* InterfaceHeader (InterfaceBody | TypeSpecifier ";")
InterfaceBody: "{" Declaration* "}"
InterfaceHeader: "interface" TypeName TypeParams? InterfaceInheritance TypeConstraints?
InterfaceInheritance: CaseTypes? Metatypes? AdaptedTypes? SatisfiedTypes?
IntersectionType: EntryType ("&" EntryType)*
Introduction: "adapt" Type SatisfiedTypes TypeConstraints? ";"
Invocation: Primary Arguments | SequenceInstantiation
IsCondition: "is" (TypedVariable Specifier | UnionType MemberName)
IteratorVariable: Variable | CallableVariable | EntryVariablePair
LIdentifier: LowercaseChar IdentifierChar*
LineComment: ("//"|"#!") ~(Newline|Return)* (Return Newline | Return | Newline)?
Literal: IntegerLiteral | FloatLiteral | CharacterLiteral | StringLiteral | QuotedLiteral
LocalNamedArgument: (UnionType | "value") MemberName (Block | NamedArguments)
LoopCondition: "while" "(" Condition ")"
LowercaseChar: "a".."z" | "_" ;
Magnitude: "k" | "M" | "G" | "T" | "P"
MemberName: LIdentifier
MemberReference: CallableReference | ValueReference
Meta: TypeMeta | MethodMeta | AttributeMeta | FunctionMeta | ValueMeta
Metatypes: "is" Type ("&" Type)*
Method: Annotation* MethodHeader (Block | NamedArguments | Specifier? ";")
MethodAttributeAlias: MemberName "="
MethodHeader: (UnionType | "function" | "void") MemberName TypeParams? Params+ Metatypes? TypeConstraints?
MethodMeta: Type "." MemberName TypeArguments?
MethodReference: (Receiver ".")? MemberName TypeArguments?
MultilineComment: "/" "*" ( MultilineCommmentCharacter | MultilineComment )* "*" "/"
MultilineCommmentCharacter: ~("/"|"*") | ("/" ~"*") => "/" | ("*" ~"/") => "*"
NamedArguments: "{" NamedArgument* Sequence? "}"
NamedArgument: SpecifiedNamedArgument | LocalNamedArgument | FunctionalNamedArgument | Object
Object: Annotation* ObjectHeader ClassBody
ObjectHeader: "object" MemberName ObjectInheritance
ObjectInheritance: ExtendedType? SatisfiedTypes?
OuterReference: (Receiver ".")? "outer"
PackageName: PIdentifier
Param: Annotation* (SimpleParam | CallableParam | EntryParamPair)
Params:  "(" Param ("," Param)* ("," DefaultParam)* ("," SequencedParam)? |  DefaultParam ("," DefaultParam)* ("," SequencedParam)? |  SequencedParam? ")"
ParenDimension: "(" Dimension ")"
ParExpression: "(" Expression ")"
PIdentifier: LowercaseChar+
PositionalArguments: "(" Expression ("," Expression)* ("," Sequence)? | Sequence? ")"
Primary: Atom | Meta | MemberReference | Invocation
QuotedLiteralCharacter: ~("'")
QuotedLiteral: "'" QuotedLiteralCharacter* "'"
Receiver: Primary
Resource: MemberName | InitializerReference Arguments | Variable Specifier
Retry: "retry"
Return: "return" Expression?
SatisfiedTypes: "satisfies" Type ("&" Type)*
SatisfiesCondition: "satisfies" Type Type
SelfReference: "this" | "super" | "outer"
SequencedParam: Annotation* UnionType "..." MemberName
SequencedTypeParam: TypeName "..."
SequencedType: TypeName "..."
Sequence: Expression ("," Expression)* | Expression "..."
SequenceInstantiation: "{" Sequence? "}" ;
SimpleAttribute: AttributeHeader ( (Specifier | Initializer)? ";" | NamedArguments )
SimpleParam: UnionType MemberName
Specification: MemberName Specifier ";"
SpecifiedNamedArgument: MemberName Specifier ";"
Specifier: "=" Expression
Statement: ExpressionStatement | Specification | DirectiveStatement | ControlStructure
StringCharacter: ~( "\" | "\"" ) | EscapeSequence
StringLiteral: "\"" StringCharacter* "\""
StringTemplate: StringLiteral (Expression StringLiteral)+
Subtype: "subtype" | MemberName "." "subtype"
SwitchCaseElse: Switch ( Cases | "{" Cases "}" )
Switch: "switch" "(" Expression ")"
Throw: "throw" Expression?
TimeLiteral:  "'"  Digit{1,2} ":" Digit{2} ( ":" Digit{2} ( ":" Digit{3} )? )?  (" " "AM"|"PM")?  (" " Character{3,4})?  "'"
ToplevelDeclaration: TypeDeclaration | Method | SimpleAttribute | AttributeGetter
TryCatchFinally: Try Catch* Finally?
Try: "try" ("(" Resource ")")? Block
TypeAlias: TypeName "="
TypeArguments: "<" (UnionType ",")* (UnionType | SequencedType) ">"
TypeArgument: UnionType | Dimension
TypeConstraint: "given" TypeName TypeParams? Params? TypeConstraintInheritance
TypeConstraintInheritance: CaseTypes? Metatypes? SatisfiedTypes? AbstractedType?
TypeConstraints: TypeConstraint+
TypeDeclaration: Class | Object | Interface
TypedQuotedLiteral: TypeName QuotedLiteral
TypedVariable: UnionType MemberName
TypeMeta: Type
TypeName: UIdentifier
TypeNameWithArguments: TypeName TypeArguments?
TypeParams: "<" (TypeParam ",")* (TypeParam | SequencedTypeParam) ">"
TypeParam: Variance? TypeName
TypeSpecifier: "=" Type
Type: TypeNameWithArguments ("." TypeNameWithArguments)*
UIdentifier: UppercaseChar IdentifierChar*
UnionType: IntersectionType ("|" IntersectionType)*
UppercaseChar: "A".."Z" ;
ValueMeta: MemberName TypeArguments?
ValueReference: (Receiver ".")? MemberName
Variable: UnionType? MemberName
Variance: "out" | "in"
While: LoopCondition Block
Whitespace: " " | Tab | Formfeed | Newline | Return


</pre>
 */

class ParseHelper {
    private final PsiBuilder builder;

    public ParseHelper(PsiBuilder builder) {
        this.builder = builder;
    }

    /**
     * TODO: Implement
     * {@code
     * abbreviatedType : qualifiedType (DEFAULT_OP | ARRAY)*
     * }
     */
    void parseAbbreviatedType() {
    }

    /**
     * TODO: Implement
     * {@code
     * abstractedType : ABSTRACTED_TYPE qualifiedType
     * }
     */
    void parseAbstractedType() {
    }

    /**
     * TODO: Implement
     * {@code
     * adaptedTypes : ADAPTED_TYPES qualifiedType (INTERSECTION_OP (qualifiedType ))*
     * }
     */
    void parseAdaptedTypes() {
    }

    /**
     * TODO: Implement
     * {@code
     * additiveExpression : multiplicativeExpression (additiveOperator multiplicativeExpression)*
     * }
     */
    void parseAdditiveExpression() {
    }

    /**
     * TODO: Implement
     * {@code
     * additiveOperator : SUM_OP | DIFFERENCE_OP | UNION_OP | XOR_OP | COMPLEMENT_OP
     * }
     */
    void parseAdditiveOperator() {
    }

    /**
     * TODO: Implement
     * {@code
     * annotatedDeclarationStart : annotation* declarationStart
     * }
     */
    void parseAnnotatedDeclarationStart() {
    }

    /**
     * TODO: Implement
     * {@code
     * annotation : annotationName annotationArguments
     * }
     */
    void parseAnnotation() {
    }

    /**
     * TODO: Implement
     * {@code
     * annotationArguments : arguments | literalArguments
     * }
     */
    void parseAnnotationArguments() {
    }

    /**
     * TODO: Implement
     * {@code
     * annotationName : LIDENTIFIER
     * }
     */
    void parseAnnotationName() {
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
     * TODO: Implement
     * {@code
     * arguments : positionalArguments | namedArguments
     * }
     */
    void parseArguments() {
    }

    /**
     * TODO: Implement
     * {@code
     * assignmentExpression : thenElseExpression (assignmentOperator assignmentExpression)?
     * }
     */
    void parseAssignmentExpression() {
    }

    /**
     * TODO: Implement
     * {@code
     * assignmentOperator : ASSIGN_OP | ADD_ASSIGN_OP | SUBTRACT_ASSIGN_OP | MULTIPLY_ASSIGN_OP | DIVIDE_ASSIGN_OP | REMAINDER_ASSIGN_OP | INTERSECT_ASSIGN_OP | UNION_ASSIGN_OP | XOR_ASSIGN_OP | COMPLEMENT_ASSIGN_OP | AND_ASSIGN_OP | OR_ASSIGN_OP
     * }
     */
    void parseAssignmentOperator() {
    }

    /**
     * TODO: Implement
     * {@code
     * attributeBody[StaticType type] : (namedArguments) => namedArguments | block
     * }
     */
    void parseAttributeBody() {
    }

    /**
     * TODO: Implement
     * {@code
     * base : nonstringLiteral | stringExpression | enumeration | selfReference | typeReference | memberReference | parExpression
     * }
     */
    void parseBase() {
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
     * TODO: Implement
     * {@code
     * booleanCondition : LPAREN (expression)? RPAREN
     * }
     */
    void parseBooleanCondition() {
    }

    /**
     * TODO: Implement
     * {@code
     * breakDirective : BREAK
     * }
     */
    void parseBreakDirective() {
    }

    /**
     * TODO: Implement
     * {@code
     * caseBlock : CASE_CLAUSE LPAREN caseItem RPAREN block
     * }
     */
    void parseCaseBlock() {
    }

    /**
     * TODO: Implement
     * {@code
     * caseItem : (IS_OP)=>isCaseCondition | (SATISFIES)=>satisfiesCaseCondition | matchCaseCondition
     * }
     */
    void parseCaseItem() {
    }

    /**
     * TODO: Implement
     * {@code
     * cases : (caseBlock)+ (defaultCaseBlock)?
     * }
     */
    void parseCases() {
    }

    /**
     * TODO: Implement
     * {@code
     * caseType : qualifiedType | memberName
     * }
     */
    void parseCaseType() {
    }

    /**
     * TODO: Implement
     * {@code
     * caseTypes : CASE_TYPES caseType (UNION_OP (caseType ))*
     * }
     */
    void parseCaseTypes() {
    }

    /**
     * TODO: Implement
     * {@code
     * catchBlock : CATCH_CLAUSE catchVariable block
     * }
     */
    void parseCatchBlock() {
    }

    /**
     * TODO: Implement
     * {@code
     * catchVariable : LPAREN (variable)? RPAREN
     * }
     */
    void parseCatchVariable() {
    }

    /**
     * TODO: Implement
     * {@code
     * classBody : LBRACE (declarationOrStatement)* RBRACE
     * }
     */
    void parseClassBody() {
    }

    /**
     * TODO: Implement
     * {@code
     * classDeclaration : CLASS_DEFINITION typeNameDeclaration (typeParameters)? (parameters)? (caseTypes)? (extendedType)? (satisfiedTypes)? (typeConstraints)? (classBody | (typeSpecifier)? SEMICOLON)
     * }
     */
    void parseClassDeclaration() {
    }

    /**
     * TODO: Implement
     * {@code
     * comparisonExpression : existenceEmptinessExpression (comparisonOperator existenceEmptinessExpression | typeOperator qualifiedType)? | typeOperator qualifiedType existenceEmptinessExpression
     * }
     */
    void parseComparisonExpression() {
    }

    /**
     * TODO: Implement
     * {@code
     * comparisonOperator : COMPARE_OP | SMALL_AS_OP | LARGE_AS_OP | LARGER_OP | SMALLER_OP | IN_OP
     * }
     */
    void parseComparisonOperator() {
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
     * comprehension : forComprehensionClause
     * }
     */
    void parseComprehension() {
    }

    /**
     * TODO: Implement
     * {@code
     * comprehensionClause : forComprehensionClause | ifComprehensionClause | expressionComprehensionClause
     * }
     */
    void parseComprehensionClause() {
    }

    /**
     * TODO: Implement
     * {@code
     * condition : (LPAREN EXISTS)=>existsCondition | (LPAREN NONEMPTY)=>nonemptyCondition | (LPAREN IS_OP)=>isCondition | (LPAREN SATISFIES)=>satisfiesCondition | booleanCondition
     * }
     */
    void parseCondition() {
    }

    /**
     * TODO: Implement
     * {@code
     * conjunctionExpression : logicalNegationExpression (conjunctionOperator logicalNegationExpression)*
     * }
     */
    void parseConjunctionExpression() {
    }

    /**
     * TODO: Implement
     * {@code
     * conjunctionOperator : AND_OP
     * }
     */
    void parseConjunctionOperator() {
    }

    /**
     * TODO: Implement
     * {@code
     * containment : IN_OP (expression)?
     * }
     */
    void parseContainment() {
    }

    /**
     * TODO: Implement
     * {@code
     * continueDirective : CONTINUE
     * }
     */
    void parseContinueDirective() {
    }

    /**
     * TODO: Implement
     * {@code
     * controlBlock : ((LBRACE)=> block )
     * }
     */
    void parseControlBlock() {
    }

    /**
     * TODO: Implement
     * {@code
     * controlStatement : ifElse | switchCaseElse | whileLoop | forElse | tryCatchFinally
     * }
     */
    void parseControlStatement() {
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
     * declarationKeyword : VALUE_MODIFIER | FUNCTION_MODIFIER | ASSIGN | VOID_MODIFIER | INTERFACE_DEFINITION | CLASS_DEFINITION | OBJECT_DEFINITION
     * }
     */
    void parseDeclarationKeyword() {
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
     * declarationStart : declarationKeyword | type (ELLIPSIS | LIDENTIFIER)
     * }
     */
    void parseDeclarationStart() {
    }

    /**
     * TODO: Implement
     * {@code
     * defaultCaseBlock : ELSE_CLAUSE block
     * }
     */
    void parseDefaultCaseBlock() {
    }

    /**
     * TODO: Implement
     * {@code
     * defaultExpression : negationComplementExpression (defaultOperator negationComplementExpression)*
     * }
     */
    void parseDefaultExpression() {
    }

    /**
     * TODO: Implement
     * {@code
     * defaultOperator : DEFAULT_OP
     * }
     */
    void parseDefaultOperator() {
    }

    /**
     * TODO: Implement
     * {@code
     * directive : returnDirective | throwDirective | breakDirective | continueDirective
     * }
     */
    void parseDirective() {
    }

    /**
     * TODO: Implement
     * {@code
     * directiveStatement : directive SEMICOLON
     * }
     */
    void parseDirectiveStatement() {
    }

    /**
     * TODO: Implement
     * {@code
     * disjunctionExpression : conjunctionExpression (disjunctionOperator conjunctionExpression)*
     * }
     */
    void parseDisjunctionExpression() {
    }

    /**
     * TODO: Implement
     * {@code
     * disjunctionOperator : OR_OP
     * }
     */
    void parseDisjunctionOperator() {
    }

    /**
     * TODO: Implement
     * {@code
     * elementSelectionOperator : SAFE_INDEX_OP | INDEX_OP
     * }
     */
    void parseElementSelectionOperator() {
    }

    /**
     * TODO: Implement
     * {@code
     * elseBlock : ELSE_CLAUSE (elseIf | block)
     * }
     */
    void parseElseBlock() {
    }

    /**
     * TODO: Implement
     * {@code
     * elseIf : ifElse
     * }
     */
    void parseElseIf() {
    }

    /**
     * TODO: Implement
     * {@code
     * entryType : abbreviatedType (ENTRY_OP (abbreviatedType ))?
     * }
     */
    void parseEntryType() {
    }

    /**
     * TODO: Implement
     * {@code
     * enumeration : LBRACE (expressions | comprehension)? RBRACE
     * }
     */
    void parseEnumeration() {
    }

    /**
     * TODO: Implement
     * {@code
     * equalityExpression : comparisonExpression (equalityOperator comparisonExpression)?
     * }
     */
    void parseEqualityExpression() {
    }

    /**
     * TODO: Implement
     * {@code
     * equalityOperator : EQUAL_OP | NOT_EQUAL_OP | IDENTICAL_OP
     * }
     */
    void parseEqualityOperator() {
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
     * TODO: Implement
     * {@code
     * existenceEmptinessExpression : rangeIntervalEntryExpression (existsNonemptyOperator)? | existsNonemptyOperator rangeIntervalEntryExpression
     * }
     */
    void parseExistenceEmptinessExpression() {
    }

    /**
     * TODO: Implement
     * {@code
     * existsCondition : (LPAREN EXISTS LIDENTIFIER RPAREN) => LPAREN EXISTS impliedVariable RPAREN | (LPAREN EXISTS compilerAnnotations (declarationStart|specificationStart)) => LPAREN EXISTS specifiedVariable RPAREN | LPAREN EXISTS (expression)? RPAREN
     * }
     */
    void parseExistsCondition() {
    }

    /**
     * TODO: Implement
     * {@code
     * existsNonemptyOperator : EXISTS | NONEMPTY
     * }
     */
    void parseExistsNonemptyOperator() {
    }

    /**
     * TODO: Implement
     * {@code
     * exponentiationExpression : incrementDecrementExpression (exponentiationOperator exponentiationExpression)?
     * }
     */
    void parseExponentiationExpression() {
    }

    /**
     * TODO: Implement
     * {@code
     * exponentiationOperator : POWER_OP
     * }
     */
    void parseExponentiationOperator() {
    }

    /**
     * TODO: Implement
     * {@code
     * expression : assignmentExpression
     * }
     */
    void parseExpression() {
    }

    /**
     * TODO: Implement
     * {@code
     * expressionComprehensionClause : expression |
     * }
     */
    void parseExpressionComprehensionClause() {
    }

    /**
     * TODO: Implement
     * {@code
     * expressionOrSpecificationStatement : expression (specifier)? (SEMICOLON | COMMA)
     * }
     */
    void parseExpressionOrSpecificationStatement() {
    }

    /**
     * TODO: Implement
     * {@code
     * expressions : expression (COMMA (expression ))*
     * }
     */
    void parseExpressions() {
    }

    /**
     * TODO: Implement
     * {@code
     * extendedType : EXTENDS (qualifiedType | SUPER MEMBER_OP typeReference) (positionalArguments )
     * }
     */
    void parseExtendedType() {
    }

    /**
     * TODO: Implement
     * {@code
     * failBlock : ELSE_CLAUSE block
     * }
     */
    void parseFailBlock() {
    }

    /**
     * TODO: Implement
     * {@code
     * finallyBlock : FINALLY_CLAUSE block
     * }
     */
    void parseFinallyBlock() {
    }

    /**
     * TODO: Implement
     * {@code
     * forBlock : FOR_CLAUSE forIterator controlBlock
     * }
     */
    void parseForBlock() {
    }

    /**
     * TODO: Implement
     * {@code
     * forComprehensionClause : FOR_CLAUSE forIterator comprehensionClause
     * }
     */
    void parseForComprehensionClause() {
    }

    /**
     * TODO: Implement
     * {@code
     * forElse : forBlock (failBlock)?
     * }
     */
    void parseForElse() {
    }

    /**
     * TODO: Implement
     * {@code
     * forIterator : LPAREN compilerAnnotations (var (containment | ENTRY_OP var containment)?)? RPAREN
     * }
     */
    void parseForIterator() {
    }

    /**
     * TODO: Implement
     * {@code
     * ifBlock : IF_CLAUSE condition controlBlock
     * }
     */
    void parseIfBlock() {
    }

    /**
     * TODO: Implement
     * {@code
     * ifComprehensionClause : IF_CLAUSE condition comprehensionClause
     * }
     */
    void parseIfComprehensionClause() {
    }

    /**
     * TODO: Implement
     * {@code
     * ifElse : ifBlock (elseBlock)?
     * }
     */
    void parseIfElse() {
    }

    /**
     * TODO: Implement
     * {@code
     * impliedVariable : memberName
     * }
     */
    void parseImpliedVariable() {
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
     * TODO: Implement
     * {@code
     * incrementDecrementExpression : prefixOperator incrementDecrementExpression | postfixIncrementDecrementExpression
     * }
     */
    void parseIncrementDecrementExpression() {
    }

    /**
     * TODO: Implement
     * {@code
     * index : additiveExpression
     * }
     */
    void parseIndex() {
    }

    /**
     * TODO: Implement
     * {@code
     * indexExpression : elementSelectionOperator indexOrIndexRange RBRACKET
     * }
     */
    void parseIndexExpression() {
    }

    /**
     * TODO: Implement
     * {@code
     * indexOrIndexRange : index (| '...' | '..' index)
     * }
     */
    void parseIndexOrIndexRange() {
    }

    /**
     * TODO: Implement
     * {@code
     * inferredAttributeDeclaration : VALUE_MODIFIER memberNameDeclaration ((specifier | initializer)? SEMICOLON | block)
     * }
     */
    void parseInferredAttributeDeclaration() {
    }

    /**
     * TODO: Implement
     * {@code
     * inferredGetterArgument : VALUE_MODIFIER memberNameDeclaration block
     * }
     */
    void parseInferredGetterArgument() {
    }

    /**
     * TODO: Implement
     * {@code
     * initializer : ASSIGN_OP expression
     * }
     */
    void parseInitializer() {
    }

    /**
     * TODO: Implement
     * {@code
     * inlineFunctionalArgument : memberName ((parametersStart) => parameters)? (LPAREN expression RPAREN | block)
     * }
     */
    void parseInlineFunctionalArgument() {
    }

    /**
     * TODO: Implement
     * {@code
     * interfaceBody : LBRACE (declarationOrStatement)* RBRACE
     * }
     */
    void parseInterfaceBody() {
    }

    /**
     * TODO: Implement
     * {@code
     * interfaceDeclaration : INTERFACE_DEFINITION typeNameDeclaration (typeParameters)? (caseTypes)? (adaptedTypes)? (satisfiedTypes)? (typeConstraints)? (interfaceBody | (typeSpecifier)? SEMICOLON)
     * }
     */
    void parseInterfaceDeclaration() {
    }

    /**
     * TODO: Implement
     * {@code
     * interpolatedExpressionStart : LPAREN | LBRACE | LIDENTIFIER | UIDENTIFIER | selfReference | nonstringLiteral | prefixOperatorStart
     * }
     */
    void parseInterpolatedExpressionStart() {
    }

    /**
     * TODO: Implement
     * {@code
     * intersectionType : entryType ((INTERSECTION_OP (entryType ))+)?
     * }
     */
    void parseIntersectionType() {
    }

    /**
     * TODO: Implement
     * {@code
     * isCaseCondition : IS_OP type
     * }
     */
    void parseIsCaseCondition() {
    }

    /**
     * TODO: Implement
     * {@code
     * isCondition : (LPAREN IS_OP type LIDENTIFIER RPAREN) => LPAREN IS_OP type impliedVariable RPAREN | (LPAREN IS_OP type LIDENTIFIER SPECIFY) => LPAREN IS_OP (type memberName specifier)? RPAREN | LPAREN IS_OP abbreviatedType (expression)? RPAREN
     * }
     */
    void parseIsCondition() {
    }

    /**
     * TODO: Implement
     * {@code
     * literalArgument : nonstringLiteral | stringLiteral
     * }
     */
    void parseLiteralArgument() {
    }

    /**
     * TODO: Implement
     * {@code
     * literalArguments : (literalArgument)*
     * }
     */
    void parseLiteralArguments() {
    }

    /**
     * TODO: Implement
     * {@code
     * logicalNegationExpression : notOperator logicalNegationExpression | equalityExpression
     * }
     */
    void parseLogicalNegationExpression() {
    }

    /**
     * TODO: Implement
     * {@code
     * matchCaseCondition : expressions
     * }
     */
    void parseMatchCaseCondition() {
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
     * TODO: Implement
     * {@code
     * memberNameDeclaration : memberName | typeName
     * }
     */
    void parseMemberNameDeclaration() {
        parseMemberName();
        parseTypeName();
    }

    /**
     * TODO: Implement
     * {@code
     * memberReference : memberName ((typeArgumentsStart) => typeArguments)?
     * }
     */
    void parseMemberReference() {
    }

    /**
     * TODO: Implement
     * {@code
     * memberSelectionOperator : MEMBER_OP | SAFE_MEMBER_OP | SPREAD_OP
     * }
     */
    void parseMemberSelectionOperator() {
    }

    /**
     * TODO: Implement
     * {@code
     * methodBody[StaticType type] : (namedArguments) => namedArguments | block
     * }
     */
    void parseMethodBody() {
    }

    /**
     * TODO: Implement
     * {@code
     * multiplicativeExpression : defaultExpression (multiplicativeOperator defaultExpression)*
     * }
     */
    void parseMultiplicativeExpression() {
    }

    /**
     * TODO: Implement
     * {@code
     * multiplicativeOperator : PRODUCT_OP | QUOTIENT_OP | REMAINDER_OP | INTERSECTION_OP
     * }
     */
    void parseMultiplicativeOperator() {
    }

    /**
     * TODO: Implement
     * {@code
     * namedArgument : compilerAnnotations (namedSpecifiedArgument | namedArgumentDeclaration)
     * }
     */
    void parseNamedArgument() {
    }

    /**
     * TODO: Implement
     * {@code
     * namedArgumentDeclaration : objectArgument | typedMethodOrGetterArgument | voidOrInferredMethodArgument | inferredGetterArgument
     * }
     */
    void parseNamedArgumentDeclaration() {
    }

    /**
     * TODO: Implement
     * {@code
     * namedArguments : LBRACE ((namedArgumentStart) => namedArgument)* (sequencedArgument | comprehension)? RBRACE
     * }
     */
    void parseNamedArguments() {
    }

    /**
     * TODO: Implement
     * {@code
     * namedArgumentStart : compilerAnnotation* (specificationStart | declarationStart)
     * }
     */
    void parseNamedArgumentStart() {
    }

    /**
     * TODO: Implement
     * {@code
     * namedSpecifiedArgument : memberNameDeclaration specifier SEMICOLON
     * }
     */
    void parseNamedSpecifiedArgument() {
    }

    /**
     * TODO: Implement
     * {@code
     * negationComplementExpression : unaryMinusOrComplementOperator negationComplementExpression | exponentiationExpression
     * }
     */
    void parseNegationComplementExpression() {
    }

    /**
     * TODO: Implement
     * {@code
     * nonemptyCondition : (LPAREN NONEMPTY LIDENTIFIER RPAREN) => LPAREN NONEMPTY impliedVariable RPAREN | (LPAREN NONEMPTY compilerAnnotations (declarationStart|specificationStart)) => LPAREN NONEMPTY (specifiedVariable)? RPAREN | LPAREN NONEMPTY (expression)? RPAREN
     * }
     */
    void parseNonemptyCondition() {
    }

    /**
     * TODO: Implement
     * {@code
     * nonstringLiteral : NATURAL_LITERAL | FLOAT_LITERAL | QUOTED_LITERAL | CHAR_LITERAL
     * }
     */
    void parseNonstringLiteral() {
    }

    /**
     * TODO: Implement
     * {@code
     * notOperator : NOT_OP
     * }
     */
    void parseNotOperator() {
    }

    /**
     * TODO: Implement
     * {@code
     * objectArgument : OBJECT_DEFINITION memberNameDeclaration (extendedType)? (satisfiedTypes)? (classBody | SEMICOLON)
     * }
     */
    void parseObjectArgument() {
    }

    /**
     * TODO: Implement
     * {@code
     * objectDeclaration : OBJECT_DEFINITION memberNameDeclaration (extendedType)? (satisfiedTypes)? (classBody | SEMICOLON)
     * }
     */
    void parseObjectDeclaration() {
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
     * TODO: Implement
     * {@code
     * parameter : parameterType memberName ((valueParameter)? | (parameters)+) (specifier)?
     * }
     */
    void parseParameter() {
    }

    /**
     * TODO: Implement
     * {@code
     * parameterDeclaration : compilerAnnotations annotations parameter
     * }
     */
    void parseParameterDeclaration() {
    }

    /**
     * TODO: Implement
     * {@code
     * parameters : LPAREN (parameterDeclaration (COMMA ((~(COMPILER_ANNOTATION | LIDENTIFIER | UIDENTIFIER)) => | parameterDeclaration))*)? RPAREN
     * }
     */
    void parseParameters() {
    }

    /**
     * TODO: Implement
     * {@code
     * parametersStart : LPAREN (annotatedDeclarationStart | RPAREN)
     * }
     */
    void parseParametersStart() {
    }

    /**
     * TODO: Implement
     * {@code
     * parameterType : type (ELLIPSIS)? | VOID_MODIFIER
     * }
     */
    void parseParameterType() {
    }

    /**
     * TODO: Implement
     * {@code
     * parExpression : LPAREN assignmentExpression RPAREN
     * }
     */
    void parseParExpression() {
    }

    /**
     * TODO: Implement
     * {@code
     * positionalArgument : (FUNCTION_MODIFIER|parametersStart)=> (FUNCTION_MODIFIER)? parameters ((parametersStart)=> parameters)* expression | VALUE_MODIFIER expression | expression
     * }
     */
    void parsePositionalArgument() {
    }

    /**
     * TODO: Implement
     * {@code
     * positionalArguments : LPAREN (positionalArgument (COMMA (positionalArgument ))* (ELLIPSIS)?)? (comprehension)? RPAREN
     * }
     */
    void parsePositionalArguments() {
    }

    /**
     * TODO: Implement
     * {@code
     * postfixIncrementDecrementExpression : primary (postfixOperator)*
     * }
     */
    void parsePostfixIncrementDecrementExpression() {
    }

    /**
     * TODO: Implement
     * {@code
     * postfixOperator : DECREMENT_OP | INCREMENT_OP
     * }
     */
    void parsePostfixOperator() {
    }

    /**
     * TODO: Implement
     * {@code
     * prefixOperator : DECREMENT_OP | INCREMENT_OP
     * }
     */
    void parsePrefixOperator() {
    }

    /**
     * TODO: Implement
     * {@code
     * prefixOperatorStart : DIFFERENCE_OP | INCREMENT_OP | DECREMENT_OP | COMPLEMENT_OP
     * }
     */
    void parsePrefixOperatorStart() {
    }

    /**
     * TODO: Implement
     * {@code
     * primary : base (qualifiedReference | indexExpression | arguments)*
     * }
     */
    void parsePrimary() {
    }

    /**
     * TODO: Implement
     * {@code
     * qualifiedReference TypeArgumentList typeArgumentList, boolean isMember] : memberSelectionOperator (memberReference | typeReference | (~(LIDENTIFIER|UIDENTIFIER))=>)
     * }
     */
    void parseQualifiedReference() {
    }

    /**
     * TODO: Implement
     * {@code
     * qualifiedType : typeNameWithArguments (MEMBER_OP typeNameWithArguments)*
     * }
     */
    void parseQualifiedType() {
    }

    /**
     * TODO: Implement
     * {@code
     * rangeIntervalEntryExpression : additiveExpression (rangeIntervalEntryOperator additiveExpression)?
     * }
     */
    void parseRangeIntervalEntryExpression() {
    }

    /**
     * TODO: Implement
     * {@code
     * rangeIntervalEntryOperator : RANGE_OP | ENTRY_OP
     * }
     */
    void parseRangeIntervalEntryOperator() {
    }

    /**
     * TODO: Implement
     * {@code
     * resource : LPAREN ((COMPILER_ANNOTATION|declarationStart|specificationStart) => specifiedVariable | expression) RPAREN
     * }
     */
    void parseResource() {
    }

    /**
     * TODO: Implement
     * {@code
     * returnDirective : RETURN (expression)?
     * }
     */
    void parseReturnDirective() {
    }

    /**
     * TODO: Implement
     * {@code
     * satisfiedTypes : SATISFIES qualifiedType (INTERSECTION_OP (qualifiedType ))*
     * }
     */
    void parseSatisfiedTypes() {
    }

    /**
     * TODO: Implement
     * {@code
     * satisfiesCaseCondition : SATISFIES qualifiedType
     * }
     */
    void parseSatisfiesCaseCondition() {
    }

    /**
     * TODO: Implement
     * {@code
     * satisfiesCondition : LPAREN SATISFIES (qualifiedType qualifiedType)? RPAREN
     * }
     */
    void parseSatisfiesCondition() {
    }

    /**
     * TODO: Implement
     * {@code
     * selfReference : THIS | SUPER | OUTER
     * }
     */
    void parseSelfReference() {
    }

    /**
     * TODO: Implement
     * {@code
     * sequencedArgument : compilerAnnotations expressions
     * }
     */
    void parseSequencedArgument() {
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
     * TODO: Implement
     * {@code
     * specificationStart : LIDENTIFIER '='
     * }
     */
    void parseSpecificationStart() {
    }

    /**
     * TODO: Implement
     * {@code
     * specifiedVariable : variable (specifier)?
     * }
     */
    void parseSpecifiedVariable() {
    }

    /**
     * TODO: Implement
     * {@code
     * specifier : SPECIFY expression
     * }
     */
    void parseSpecifier() {
    }

    /**
     * TODO: Implement
     * {@code
     * statement : directiveStatement | controlStatement | expressionOrSpecificationStatement
     * }
     */
    void parseStatement() {
    }

    /**
     * TODO: Implement
     * {@code
     * stringExpression : (STRING_LITERAL interpolatedExpressionStart) => stringTemplate | stringLiteral
     * }
     */
    void parseStringExpression() {
    }

    /**
     * TODO: Implement
     * {@code
     * stringLiteral : STRING_LITERAL
     * }
     */
    void parseStringLiteral() {
    }

    /**
     * TODO: Implement
     * {@code
     * stringTemplate : stringLiteral ((interpolatedExpressionStart) => expression stringLiteral)+
     * }
     */
    void parseStringTemplate() {
    }

    /**
     * TODO: Implement
     * {@code
     * switchCaseElse : switchHeader cases
     * }
     */
    void parseSwitchCaseElse() {
    }

    /**
     * TODO: Implement
     * {@code
     * switchHeader : SWITCH_CLAUSE LPAREN expression RPAREN
     * }
     */
    void parseSwitchHeader() {
    }

    /**
     * TODO: Implement
     * {@code
     * thenElseExpression : disjunctionExpression (thenElseOperator disjunctionExpression)*
     * }
     */
    void parseThenElseExpression() {
    }

    /**
     * TODO: Implement
     * {@code
     * thenElseOperator : ELSE_CLAUSE | THEN_CLAUSE
     * }
     */
    void parseThenElseOperator() {
    }

    /**
     * TODO: Implement
     * {@code
     * throwDirective : THROW (expression)?
     * }
     */
    void parseThrowDirective() {
    }

    /**
     * TODO: Implement
     * {@code
     * tryBlock : TRY_CLAUSE (resource controlBlock | block)
     * }
     */
    void parseTryBlock() {
    }

    /**
     * TODO: Implement
     * {@code
     * tryCatchFinally : tryBlock (catchBlock)* (finallyBlock)?
     * }
     */
    void parseTryCatchFinally() {
    }

    /**
     * TODO: Implement
     * {@code
     * type : unionType
     * }
     */
    void parseType() {
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
     * typeArgument : type (ELLIPSIS)?
     * }
     */
    void parseTypeArgument() {
    }

    /**
     * TODO: Implement
     * {@code
     * typeArguments : SMALLER_OP typeArgument (COMMA (typeArgument ))* LARGER_OP
     * }
     */
    void parseTypeArguments() {
    }

    /**
     * TODO: Implement
     * {@code
     * typeArgumentsStart : SMALLER_OP (UIDENTIFIER ('.' UIDENTIFIER)* (DEFAULT_OP|ARRAY)* ((INTERSECTION_OP|UNION_OP|ENTRY_OP) UIDENTIFIER ('.' UIDENTIFIER)* (DEFAULT_OP|ARRAY)*)* (LARGER_OP|SMALLER_OP|COMMA|ELLIPSIS) | LARGER_OP)
     * }
     */
    void parseTypeArgumentsStart() {
    }

    /**
     * TODO: Implement
     * {@code
     * typeConstraint : compilerAnnotations TYPE_CONSTRAINT typeNameDeclaration (parameters)? (caseTypes)? (satisfiedTypes)? (abstractedType)?
     * }
     */
    void parseTypeConstraint() {
    }

    /**
     * TODO: Implement
     * {@code
     * typeConstraints : (typeConstraint)+
     * }
     */
    void parseTypeConstraints() {
    }

    /**
     * TODO: Implement
     * {@code
     * typedMethodOrAttributeDeclaration : type memberNameDeclaration ((typeParameters)? (parameters)+ (typeConstraints)? (methodBody[$type.type] | (specifier)? SEMICOLON) | (specifier | initializer)? SEMICOLON | attributeBody[$type.type])
     * }
     */
    void parseTypedMethodOrAttributeDeclaration() {
    }

    /**
     * TODO: Implement
     * {@code
     * typedMethodOrGetterArgument : type memberNameDeclaration ((parameters)+)? methodBody[$type.type]
     * }
     */
    void parseTypedMethodOrGetterArgument() {
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

    /**
     * TODO: Implement
     * {@code
     * typeNameDeclaration : typeName | memberName
     * }
     */
    void parseTypeNameDeclaration() {
    }

    /**
     * TODO: Implement
     * {@code
     * typeNameWithArguments : typeName (typeArguments)?
     * }
     */
    void parseTypeNameWithArguments() {
    }

    /**
     * TODO: Implement
     * {@code
     * typeOperator : IS_OP | EXTENDS | SATISFIES
     * }
     */
    void parseTypeOperator() {
    }

    /**
     * TODO: Implement
     * {@code
     * typeParameter : (variance)? typeNameDeclaration | typeNameDeclaration ELLIPSIS
     * }
     */
    void parseTypeParameter() {
    }

    /**
     * TODO: Implement
     * {@code
     * typeParameters : SMALLER_OP typeParameter (COMMA (typeParameter ))* LARGER_OP
     * }
     */
    void parseTypeParameters() {
    }

    /**
     * TODO: Implement
     * {@code
     * typeReference : typeName ((typeArgumentsStart) => typeArguments)?
     * }
     */
    void parseTypeReference() {
    }

    /**
     * TODO: Implement
     * {@code
     * typeSpecifier : SPECIFY qualifiedType
     * }
     */
    void parseTypeSpecifier() {
    }

    /**
     * TODO: Implement
     * {@code
     * unaryMinusOrComplementOperator : DIFFERENCE_OP | SUM_OP | COMPLEMENT_OP
     * }
     */
    void parseUnaryMinusOrComplementOperator() {
    }

    /**
     * TODO: Implement
     * {@code
     * unionType : intersectionType ((UNION_OP (intersectionType ))+)?
     * }
     */
    void parseUnionType() {
    }

    /**
     * TODO: Implement
     * {@code
     * valueParameter : ENTRY_OP type memberName
     * }
     */
    void parseValueParameter() {
    }

    /**
     * TODO: Implement
     * {@code
     * var : (type memberName (parameters)* | memberName | memberName (parameters)+)
     * }
     */
    void parseVar() {
    }

    /**
     * TODO: Implement
     * {@code
     * variable : compilerAnnotations var
     * }
     */
    void parseVariable() {
    }

    /**
     * TODO: Implement
     * {@code
     * variance : IN_OP | OUT
     * }
     */
    void parseVariance() {
    }

    /**
     * TODO: Implement
     * {@code
     * voidOrInferredMethodArgument : (VOID_MODIFIER | FUNCTION_MODIFIER) memberNameDeclaration (parameters)* block
     * }
     */
    void parseVoidOrInferredMethodArgument() {
    }

    /**
     * TODO: Implement
     * {@code
     * voidOrInferredMethodDeclaration : (VOID_MODIFIER | FUNCTION_MODIFIER) memberNameDeclaration (typeParameters)? (parameters)* (typeConstraints)? (block | (specifier)? SEMICOLON)
     * }
     */
    void parseVoidOrInferredMethodDeclaration() {
    }

    /**
     * TODO: Implement
     * {@code
     * whileBlock : WHILE_CLAUSE condition controlBlock
     * }
     */
    void parseWhileBlock() {
    }


    /**
     * TODO: Implement
     * {@code
     * whileLoop : whileBlock
     * }
     */
    void parseWhileLoop() {
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
