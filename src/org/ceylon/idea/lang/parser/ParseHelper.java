package org.ceylon.idea.lang.parser;

import com.intellij.lang.PsiBuilder;
import com.intellij.psi.tree.IElementType;
import org.ceylon.idea.lang.lexer.CeylonToken;

/*
<pre>

ToplevelDeclaration: TypeDeclaration | Method | SimpleAttribute | AttributeGetter
TypeDeclaration: Class | Object | Interface
Declaration: Method | Attribute | TypeDeclaration
Import: "import" FullPackageName "{" ImportElements? "}"
FullPackageName: PackageName ("." PackageName)*
ImportElements: ImportElement ("," ImportElement)* ("," ImportWildcard)? | ImportWildcard
ImportElement: ImportTypeElement | ImportMethodAttributeElement
ImportTypeElement: TypeAlias? TypeName
ImportMethodAttributeElement: MethodAttributeAlias? MemberName
TypeAlias: TypeName "="
MethodAttributeAlias: MemberName "="
ImportWildcard: "..."
Introduction: "adapt" Type SatisfiedTypes TypeConstraints? ";"
Interface: Annotation* InterfaceHeader (InterfaceBody | TypeSpecifier ";")
InterfaceHeader: "interface" TypeName TypeParams? InterfaceInheritance TypeConstraints?
InterfaceInheritance: CaseTypes? Metatypes? AdaptedTypes? SatisfiedTypes?
InterfaceBody: "{" Declaration* "}"
TypeSpecifier: "=" Type
Class: Annotation* ClassHeader (ClassBody | TypeSpecifier ";")
ClassHeader: "class" TypeName TypeParams? Params ClassInheritance TypeConstraints?
ClassInheritance: CaseTypes? Metatypes? ExtendedType? SatisfiedTypes?
ClassBody: "{" (Declaration | Statement)* "}"
Object: Annotation* ObjectHeader ClassBody
ObjectHeader: "object" MemberName ObjectInheritance
ObjectInheritance: ExtendedType? SatisfiedTypes?
Method: Annotation* MethodHeader (Block | NamedArguments | Specifier? ";")
MethodHeader: (UnionType | "function" | "void") MemberName TypeParams? Params+ Metatypes? TypeConstraints?
Attribute: Annotation* (SimpleAttribute | AttributeGetter | AttributeSetter)
AttributeHeader: (UnionType | "value") MemberName
SimpleAttribute: AttributeHeader ( (Specifier | Initializer)? ";" | NamedArguments )
Initializer: ":=" Expression
AttributeGetter: AttributeHeader Block
AttributeSetter: "assign" MemberName Block
Literal: IntegerLiteral | FloatLiteral | CharacterLiteral | StringLiteral | QuotedLiteral
DateLiteral:  "'"  Digit{1,2} "/" Digit{1,2} "/" Digit{4}  "'"
TimeLiteral:  "'"  Digit{1,2} ":" Digit{2} ( ":" Digit{2} ( ":" Digit{3} )? )?  (" " "AM"|"PM")?  (" " Character{3,4})?  "'"
TypedQuotedLiteral: TypeName QuotedLiteral
StringTemplate: StringLiteral (Expression StringLiteral)+
SelfReference: "this" | "super" | "outer"
Atom: Literal | StringTemplate | SelfReference | ParExpression
Primary: Atom | Meta | MemberReference | Invocation
MemberReference: CallableReference | ValueReference
Expression: Primary | OperatorExpression
ParExpression: "(" Expression ")"
Receiver: Primary
OuterReference: (Receiver ".")? "outer"
ValueReference: (Receiver ".")? MemberName
CallableReference: MethodReference | InitializerReference
MethodReference: (Receiver ".")? MemberName TypeArguments?
InitializerReference: (Receiver ".")? TypeName TypeArguments?
Invocation: Primary Arguments | SequenceInstantiation
Arguments: PositionalArguments FunctionalArguments? | NamedArguments
PositionalArguments: "(" Expression ("," Expression)* ("," Sequence)? | Sequence? ")"
NamedArguments: "{" NamedArgument* Sequence? "}"
NamedArgument: SpecifiedNamedArgument | LocalNamedArgument | FunctionalNamedArgument | Object
SpecifiedNamedArgument: MemberName Specifier ";"
LocalNamedArgument: (UnionType | "value") MemberName (Block | NamedArguments)
FunctionalNamedArgument: (UnionType | "function" | "void") MemberName Params+ (Block | NamedArguments)
SequenceInstantiation: "{" Sequence? "}" ;
Sequence: Expression ("," Expression)* | Expression "..."
FunctionalArguments: (MemberName FunctionalBody)+
FunctionalBody: Params? ( Block | "(" Expression ")" )
Meta: TypeMeta | MethodMeta | AttributeMeta | FunctionMeta | ValueMeta
TypeMeta: Type
FunctionMeta: MemberName TypeArguments?
MethodMeta: Type "." MemberName TypeArguments?
ValueMeta: MemberName TypeArguments?
AttributeMeta: Type "." MemberName
Whitespace: " " | Tab | Formfeed | Newline | Return
LineComment: ("//"|"#!") ~(Newline|Return)* (Return Newline | Return | Newline)?
MultilineComment: "/" "*" ( MultilineCommmentCharacter | MultilineComment )* "*" "/"
MultilineCommmentCharacter: ~("/"|"*") | ("/" ~"*") => "/" | ("*" ~"/") => "*"
IdentifierChar: LowercaseChar | UppercaseChar | Digit
Digit: "0".."9"
LowercaseChar: "a".."z" | "_" ;
UppercaseChar: "A".."Z" ;
LIdentifier: LowercaseChar IdentifierChar*
UIdentifier: UppercaseChar IdentifierChar*
PIdentifier: LowercaseChar+
IntegerLiteral: Digits Magnitude?
FloatLiteral: Digits ("." FractionalDigits (Exponent | Magnitude | FractionalMagnitude)? | FractionalMagnitude)
Digits: Digit+ | Digit{1..3} ("_" Digit{3})+
FractionalDigits: Digit+ | (Digit{3} "_")+ Digit{1..3}
Exponent: ("E"|"e") ("+"|"-")? Digits
Magnitude: "k" | "M" | "G" | "T" | "P"
FractionalMagnitude: "m" | "u" | "n" | "p" | "f"
CharacterLiteral: "`" Character "`"
Character: ~("`" | "\" | Tab | Formfeed | Newline | Return | Backspace) | EscapeSequence
EscapeSequence: "\" ("b" | "t" | "n" | "f" | "r" | "\" | "\"" | "'" | "`" )
StringLiteral: "\"" StringCharacter* "\""
StringCharacter: ~( "\" | "\"" ) | EscapeSequence
QuotedLiteral: "'" QuotedLiteralCharacter* "'"
QuotedLiteralCharacter: ~("'")
Block: "{" (Declaration | Statement)* "}"
Statement: ExpressionStatement | Specification | DirectiveStatement | ControlStructure
ExpressionStatement: ( Assignment | IncrementOrDecrement | Invocation ) ";"
Assignment: ":=" | ".=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^="| "~=" | "&&=" | "||=" ;
IncrementOrDecrement: "--" | "++" ;
DirectiveStatement: Directive ";"
Directive: Return | Throw | Break | Continue
Return: "return" Expression?
Break: "break"
Continue: "continue"
Throw: "throw" Expression?
Retry: "retry"
Specification: MemberName Specifier ";"
ControlStructure: IfElse | SwitchCaseElse | While | ForFail | TryCatchFinally
TypedVariable: UnionType MemberName
Variable: UnionType? MemberName
IteratorVariable: Variable | CallableVariable | EntryVariablePair
CallableVariable: (UnionType | "void")? MemberName Params+
EntryVariablePair: Variable "->" Variable
Condition: BooleanCondition | IsCondition | ExistsOrNonemptyCondition | SatisfiesCondition
BooleanCondition: Expression
IsCondition: "is" (TypedVariable Specifier | UnionType MemberName)
ExistsOrNonemptyCondition: ("exists" | "nonempty") (Variable Specifier | MemberName)
SatisfiesCondition: "satisfies" Type Type
IfElse: If Else?
If: "if" "(" Condition ")" Block
Else: "else" (Block | IfElse)
SwitchCaseElse: Switch ( Cases | "{" Cases "}" )
Switch: "switch" "(" Expression ")"
Cases: CaseItem+ DefaultCaseItem?
CaseItem: "case" "(" Case ")" Block
DefaultCaseItem: "else" Block
Case: Expression ("," Expression)* | "is" UnionType | "satisfies" Type
ForFail: For Fail?
For: "for" "(" ForIterator ")" Block
Fail: "else" Block
ForIterator: IteratorVariable "in" Expression
While: LoopCondition Block
LoopCondition: "while" "(" Condition ")"
TryCatchFinally: Try Catch* Finally?
Try: "try" ("(" Resource ")")? Block
Catch: "catch" "(" Variable ")" Block
Finally: "finally" Block
Resource: MemberName | InitializerReference Arguments | Variable Specifier
PackageName: PIdentifier
TypeName: UIdentifier
MemberName: LIdentifier
UnionType: IntersectionType ("|" IntersectionType)*
IntersectionType: EntryType ("&" EntryType)*
TypeNameWithArguments: TypeName TypeArguments?
Type: TypeNameWithArguments ("." TypeNameWithArguments)*
AbbreviatedType: Type Abbreviation*
Abbreviation: "?" | "[]"
EntryType: AbbreviatedType ("->" AbbreviatedType)?
ExtendedType: "extends" ("super" ".")? Type PositionalArguments
SatisfiedTypes: "satisfies" Type ("&" Type)*
ConditionalTypes: SatisfiedTypes Conditions
Conditions: "if" "(" Condition ("&&" Condition)* ")"
CaseTypes: "of" CaseType ("|" CaseType)*
CaseType: MemberName | Type
AdaptedTypes: "adapts" Type ("&" Type)*
Metatypes: "is" Type ("&" Type)*
TypeParams: "<" (TypeParam ",")* (TypeParam | SequencedTypeParam) ">"
TypeParam: Variance? TypeName
Variance: "out" | "in"
SequencedTypeParam: TypeName "..."
Subtype: "subtype" | MemberName "." "subtype"
TypeConstraints: TypeConstraint+
TypeConstraint: "given" TypeName TypeParams? Params? TypeConstraintInheritance
TypeConstraintInheritance: CaseTypes? Metatypes? SatisfiedTypes? AbstractedType?
AbstractedType: "abstracts" Type
ConcreteType: "this" "is"
TypeArguments: "<" (UnionType ",")* (UnionType | SequencedType) ">"
SequencedType: TypeName "..."
TypeArgument: UnionType | Dimension
Dimension: DimensionTerm ("+" DimensionTerm)*
DimensionTerm: (DimensionConstant "*")* DimensionAtom
DimensionAtom: DimensionConstant | DimensionVariable | ParenDimension
ParenDimension: "(" Dimension ")"
DimensionConstant: "#" IntegerLiteral
DimensionVariable: TypeName | "#" MemberName
Params:  "(" Param ("," Param)* ("," DefaultParam)* ("," SequencedParam)? |  DefaultParam ("," DefaultParam)* ("," SequencedParam)? |  SequencedParam? ")"
Param: Annotation* (SimpleParam | CallableParam | EntryParamPair)
SimpleParam: UnionType MemberName
CallableParam: (UnionType | "void") MemberName Params+
DefaultParam: Param Specifier
Specifier: "=" Expression
SequencedParam: Annotation* UnionType "..." MemberName
EntryParamPair: SimpleParam "->" SimpleParam
Annotation: MemberName ( Arguments | Literal+ )?

</pre>
 */

public class ParseHelper {
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
     * TODO: Implement
     * {@code
     * block : LBRACE (declarationOrStatement)* RBRACE
     * }
     */
    boolean parseBlock() {
        if (!ParserUtils.lookAhead(builder, CeylonToken.LBRACE)) {
            return false;
        }

        PsiBuilder.Marker marker = builder.mark();

        getToken(CeylonToken.LBRACE);

        while (parseDeclarationOrStatement()) {
        }

        getToken(CeylonToken.RBRACE, "RBRACE expected");

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
            getToken(CeylonToken.SEMICOLON, "SEMICOLON expected");
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
        if (!ParserUtils.lookAhead(builder, CeylonToken.COMPILER_ANNOTATION)) {
            return false;
        }

        PsiBuilder.Marker marker = builder.mark();

        getToken(CeylonToken.COMPILER_ANNOTATION);
        getToken(CeylonToken.LIDENTIFIER, "LIDENTIFIER expected");

        if (getToken(CeylonToken.INDEX_OP)) {
            getToken(CeylonToken.STRING_LITERAL, "STRING_LITERAL expected");
            getToken(CeylonToken.RBRACKET, "RBRACKET expected");
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
     * {@code
     * importDeclaration : IMPORT fullPackageName importElementList
     * }
     */
    boolean parseImportDeclaration() {
        if (!lookAhead(CeylonToken.IMPORT)) {
            return false;
        }

        PsiBuilder.Marker marker = builder.mark();

        getToken(CeylonToken.IMPORT);

        parseFullPackageName();

        parseImportElementList();

        marker.done(CeylonAstNode.IMPORT);
        return true;
    }

    /**
     * {@code
     * fullPackageName : packageName (MEMBER_OP packageName)*
     * }
     */
    void parseFullPackageName() {
        PsiBuilder.Marker marker = builder.mark();

        parsePackageName();

        while (getToken(CeylonToken.MEMBER_OP)) {
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

        if (lookAhead(CeylonToken.LIDENTIFIER)) {
            parseMemberAlias();
            parseMemberName();
            parseErasure();
        } else if (lookAhead(CeylonToken.UIDENTIFIER)) {
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
    void parseImportElementList() {
        PsiBuilder.Marker marker = builder.mark();

        getToken(CeylonToken.LBRACE, "LBRACE expected");

        if (!parseImportWildcard()) {
            if (parseImportElement()) {
                while (getToken(CeylonToken.COMMA)) {
                    if (parseImportWildcard()) {
                        break;
                    } else {
                        parseImportElement();
                    }
                }
            }
        }

        getToken(CeylonToken.RBRACE, "RBRACE expected");

        marker.done(CeylonAstNode.IMPORT_MEMBER_OR_TYPE_LIST);
    }

    /**
     * {@code
     * importList : (importDeclaration)*
     * }
     */
    void parseImportList() {
        if (!ParserUtils.lookAhead(builder, CeylonToken.IMPORT)) {
            return;
        }

        PsiBuilder.Marker marker = builder.mark();

        while (parseImportDeclaration()) {
        }

        marker.done(CeylonAstNode.IMPORT_LIST);
    }

    /**
     * {@code
     * importWildcard : ELLIPSIS
     * }
     */
    boolean parseImportWildcard() {
        if (!ParserUtils.lookAhead(builder, CeylonToken.ELLIPSIS)) {
            return false;
        }

        PsiBuilder.Marker marker = builder.mark();

        getToken(CeylonToken.ELLIPSIS);

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
        if (getToken(CeylonToken.SPECIFY)) {
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
        getToken(CeylonToken.LIDENTIFIER, "LIDENTIFIER expected");
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
        getToken(CeylonToken.LIDENTIFIER, "packageName expected");
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
        if (!ParserUtils.lookAhead(builder, CeylonToken.ASSIGN)) {
            return false;
        }

        PsiBuilder.Marker marker = builder.mark();

        getToken(CeylonToken.ASSIGN);

        parseMemberName();

        if (!parseBlock() && !getToken(CeylonToken.SEMICOLON)) {
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
        if (getToken(CeylonToken.SPECIFY)) {
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
        getToken(CeylonToken.UIDENTIFIER, "UIDENTIFIER expected");
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

    private boolean getToken(IElementType elem) {
        return ParserUtils.getToken(builder, elem);
    }

    private boolean getToken(IElementType elem, String errorMsg) {
        return ParserUtils.getToken(builder, elem, errorMsg);
    }

    private boolean lookAhead(IElementType... elems) {
        return ParserUtils.lookAhead(builder, elems);
    }

}
