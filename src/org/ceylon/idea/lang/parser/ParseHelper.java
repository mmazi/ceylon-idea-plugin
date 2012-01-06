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
