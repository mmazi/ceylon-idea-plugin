package org.ceylon.idea.lang.parser;

import org.ceylon.idea.lang.lexer.CeylonToken;
import org.ceylon.idea.lang.parser.rule.MockPsiBuilder;
import org.ceylon.idea.lang.parser.rule.Rule;
import org.junit.Test;

import static org.ceylon.idea.lang.lexer.CeylonToken.*;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class GrammarTest {
    private final MockPsiBuilder builder = new MockPsiBuilder();

    @Test
    public void testAbbreviatedType() {
        // AbbreviatedType: Type Abbreviation*
        Rule rule = Grammar.AbbreviatedType;

        valid(rule, UIDENTIFIER);
        valid(rule, UIDENTIFIER, QMARK);
        valid(rule, UIDENTIFIER, ARRAY);
        valid(rule, UIDENTIFIER, QMARK, ARRAY);
    }

    @Test
    public void testAbstractedType() {
        // AbstractedType: "abstracts" Type
        Rule rule = Grammar.AbstractedType;

        // TODO: Implement
    }

    @Test
    public void testAdaptedTypes() {
        // AdaptedTypes: "adapts" Type ("&" Type)*
        Rule rule = Grammar.AdaptedTypes;

        // TODO: Implement
    }

    @Test
    public void testAnnotation() {
        // Annotation: MemberName ( Arguments | Literal+ )?
        Rule rule = Grammar.Annotation;

        valid(rule, LIDENTIFIER);
        valid(rule, LIDENTIFIER, STRING_LITERAL);
        valid(rule, LIDENTIFIER, STRING_LITERAL, STRING_LITERAL);
        valid(rule, LIDENTIFIER, LBRACE, RBRACE);
        valid(rule, LIDENTIFIER, LBRACE, LIDENTIFIER, SPECIFY, THIS, SEMICOLON, RBRACE);
    }

    @Test
    public void testAnnotationName() {
        // AnnotationName: LIdentifier
        Rule rule = Grammar.AnnotationName;

        // TODO: Implement
    }

    @Test
    public void testArguments() {
        // Arguments: PositionalArguments FunctionalArguments? | NamedArguments
        Rule rule = Grammar.Arguments;

        // TODO: Implement
    }

    @Test
    public void testAssignment() {
        // Assignment: ":=" | ".=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^="| "~=" | "&&=" | "||=" ;
        Rule rule = Grammar.Assignment;

        // TODO: Implement
    }

    @Test
    public void testAtom() {
        // Atom: Literal | StringTemplate | SelfReference | ParExpression
        Rule rule = Grammar.Atom;

        // TODO: Implement
    }

    @Test
    public void testAttribute() {
        // Attribute: Annotation* (SimpleAttribute | AttributeGetter | AttributeSetter)
        Rule rule = Grammar.Attribute;

        // TODO: Implement
    }

    @Test
    public void testAttributeGetter() {
        // AttributeGetter:   (UnionType | "value") MemberName Block
        Rule rule = Grammar.AttributeGetter;

        valid(rule, VALUE_MODIFIER, LIDENTIFIER, LBRACE, RBRACE);
        valid(rule, UIDENTIFIER, ARRAY, LIDENTIFIER, LBRACE, RBRACE);
        valid(rule, UIDENTIFIER, UNION_OP, UIDENTIFIER, QMARK, LIDENTIFIER, LBRACE, RBRACE);
    }

    @Test
    public void testAttributeHeader() {
        // AttributeHeader: (UnionType | "value") MemberName
        Rule rule = Grammar.AttributeHeader;

        // TODO: Implement
    }

    @Test
    public void testAttributeMeta() {
        // AttributeMeta: Type "." MemberName
        Rule rule = Grammar.AttributeMeta;

        // TODO: Implement
    }

    @Test
    public void testAttributeSetter() {
        // AttributeSetter:  "assign" MemberName Block
        Rule rule = Grammar.AttributeSetter;

        valid(rule, ASSIGN, LIDENTIFIER, LBRACE, RBRACE);
    }

    @Test
    public void testBlock() {
        // Block: "{" (Declaration | Statement)* "}"
        Rule rule = Grammar.Block;

        // TODO: Implement
    }

    @Test
    public void testBooleanCondition() {
        // BooleanCondition: Expression
        Rule rule = Grammar.BooleanCondition;

        // TODO: Implement
    }

    @Test
    public void testBreak() {
        // Break: "break"
        Rule rule = Grammar.Break;

        // TODO: Implement
    }

    @Test
    public void testCallableParam() {
        // CallableParam: (UnionType | "void") MemberName Params+
        Rule rule = Grammar.CallableParam;

        // TODO: Implement
    }

    @Test
    public void testCallableReference() {
        // CallableReference: MethodReference | InitializerReference
        Rule rule = Grammar.CallableReference;

        // TODO: Implement
    }

    @Test
    public void testCallableVariable() {
        // CallableVariable: (UnionType | "void")? MemberName Params+
        Rule rule = Grammar.CallableVariable;

        // TODO: Implement
    }

    @Test
    public void testCase() {
        // Case: Expression ("," Expression)* | "is" UnionType | "satisfies" Type
        Rule rule = Grammar.Case;

        // TODO: Implement
    }

    @Test
    public void testCaseItem() {
        // CaseItem: "case" "(" Case ")" Block
        Rule rule = Grammar.CaseItem;

        // TODO: Implement
    }

    @Test
    public void testCases() {
        // Cases: CaseItem+ DefaultCaseItem?
        Rule rule = Grammar.Cases;

        // TODO: Implement
    }

    @Test
    public void testCaseType() {
        // CaseType: MemberName | Type
        Rule rule = Grammar.CaseType;

        // TODO: Implement
    }

    @Test
    public void testCaseTypes() {
        // CaseTypes: "of" CaseType ("|" CaseType)*
        Rule rule = Grammar.CaseTypes;

        // TODO: Implement
    }

    @Test
    public void testCatch() {
        // Catch: "catch" "(" Variable ")" Block
        Rule rule = Grammar.Catch;

        // TODO: Implement
    }

    @Test
    public void testCharacter() {
        // Character: ~("`" | "\" | Tab | Formfeed | Newline | Return | Backspace) | EscapeSequence
        Rule rule = Grammar.Character;

        // TODO: Implement
    }

    @Test
    public void testCharacterLiteral() {
        // CharacterLiteral: "`" Character "`"
        Rule rule = Grammar.CharacterLiteral;

        // TODO: Implement
    }

    @Test
    public void testClass() {
        // Class: Annotation* ClassHeader (ClassBody | TypeSpecifier ";")
        Rule rule = Grammar.Class;

        // TODO: Implement
    }

    @Test
    public void testClassBody() {
        // ClassBody: "{" (Declaration | Statement)* "}"
        Rule rule = Grammar.ClassBody;

        // TODO: Implement
    }

    @Test
    public void testClassHeader() {
        // ClassHeader: "class" TypeName TypeParams? Params ClassInheritance TypeConstraints?
        Rule rule = Grammar.ClassHeader;

        // TODO: Implement
    }

    @Test
    public void testClassInheritance() {
        // ClassInheritance: CaseTypes? Metatypes? ExtendedType? SatisfiedTypes?
        Rule rule = Grammar.ClassInheritance;

        // TODO: Implement
    }

    @Test
    public void testCompilationUnit() {
        // CompilationUnit : (CompilerAnnotation+ ";")? import* (CompilerAnnotations Declaration)*
        Rule rule = Grammar.CompilationUnit;

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

    @Test
    public void testCompilerAnnotation() {
        // CompilerAnnotation : "@" AnnotationName ("[" StringLiteral "]")?
        Rule rule = Grammar.CompilerAnnotation;

        // TODO: Implement
    }

    @Test
    public void testCompilerAnnotations() {
        // CompilerAnnotations: CompilerAnnotation*
        Rule rule = Grammar.CompilerAnnotations;

        // TODO: Implement
    }

    @Test
    public void testConcreteType() {
        // ConcreteType: "this" "is"
        Rule rule = Grammar.ConcreteType;

        // TODO: Implement
    }

    @Test
    public void testCondition() {
        // Condition: BooleanCondition | IsCondition | ExistsOrNonemptyCondition | SatisfiesCondition
        Rule rule = Grammar.Condition;

        // TODO: Implement
    }

    @Test
    public void testConditionalTypes() {
        // ConditionalTypes: SatisfiedTypes Conditions
        Rule rule = Grammar.ConditionalTypes;

        // TODO: Implement
    }

    @Test
    public void testConditions() {
        // Conditions: "if" "(" Condition ("&&" Condition)* ")"
        Rule rule = Grammar.Conditions;

        // TODO: Implement
    }

    @Test
    public void testContinue() {
        // Continue: "continue"
        Rule rule = Grammar.Continue;

        // TODO: Implement
    }

    @Test
    public void testControlStructure() {
        // ControlStructure: IfElse | SwitchCaseElse | While | ForFail | TryCatchFinally
        Rule rule = Grammar.ControlStructure;

        // TODO: Implement
    }

    @Test
    public void testDateLiteral() {
        // DateLiteral: "'" Digit{1,2} "/" Digit{1,2} "/" Digit{4} "'"
        Rule rule = Grammar.DateLiteral;

        // TODO: Implement
    }

    @Test
    public void testDeclaration() {
        // Declaration: Method | Attribute | TypeDeclaration
        Rule rule = Grammar.Declaration;

        // TODO: Implement
    }

    @Test
    public void testDefaultCaseItem() {
        // DefaultCaseItem: "else" Block
        Rule rule = Grammar.DefaultCaseItem;

        // TODO: Implement
    }

    @Test
    public void testDefaultParam() {
        // DefaultParam: Param Specifier
        Rule rule = Grammar.DefaultParam;

        // TODO: Implement
    }

    @Test
    public void testDigit() {
        // Digit: "0".."9"
        Rule rule = Grammar.Digit;

        // TODO: Implement
    }

    @Test
    public void testDigits() {
        // Digits: Digit+ | Digit{1..3} ("_" Digit{3})+
        Rule rule = Grammar.Digits;

        // TODO: Implement
    }

    @Test
    public void testDimension() {
        // Dimension: DimensionTerm ("+" DimensionTerm)*
        Rule rule = Grammar.Dimension;

        // TODO: Implement
    }

    @Test
    public void testDimensionAtom() {
        // DimensionAtom: DimensionConstant | DimensionVariable | ParenDimension
        Rule rule = Grammar.DimensionAtom;

        // TODO: Implement
    }

    @Test
    public void testDimensionConstant() {
        // DimensionConstant: "#" IntegerLiteral
        Rule rule = Grammar.DimensionConstant;

        // TODO: Implement
    }

    @Test
    public void testDimensionTerm() {
        // DimensionTerm: (DimensionConstant "*")* DimensionAtom
        Rule rule = Grammar.DimensionTerm;

        // TODO: Implement
    }

    @Test
    public void testDimensionVariable() {
        // DimensionVariable: TypeName | "#" MemberName
        Rule rule = Grammar.DimensionVariable;

        // TODO: Implement
    }

    @Test
    public void testDirective() {
        // Directive: Return | Throw | Break | Continue
        Rule rule = Grammar.Directive;

        // TODO: Implement
    }

    @Test
    public void testDirectiveStatement() {
        // DirectiveStatement: Directive ";"
        Rule rule = Grammar.DirectiveStatement;

        // TODO: Implement
    }

    @Test
    public void testElse() {
        // Else: "else" (Block | IfElse)
        Rule rule = Grammar.Else;

        // TODO: Implement
    }

    @Test
    public void testEntryParamPair() {
        // EntryParamPair: SimpleParam "->" SimpleParam
        Rule rule = Grammar.EntryParamPair;

        // TODO: Implement
    }

    @Test
    public void testEntryType() {
        // EntryType: AbbreviatedType ("->" AbbreviatedType)?
        Rule rule = Grammar.EntryType;

        // TODO: Implement
    }

    @Test
    public void testEntryVariablePair() {
        // EntryVariablePair: Variable "->" Variable
        Rule rule = Grammar.EntryVariablePair;

        // TODO: Implement
    }

    @Test
    public void testEscapeSequence() {
        // EscapeSequence: "\" ("b" | "t" | "n" | "f" | "r" | "\" | "\"" | "'" | "`" )
        Rule rule = Grammar.EscapeSequence;

        // TODO: Implement
    }

    @Test
    public void testExistsOrNonemptyCondition() {
        // ExistsOrNonemptyCondition: ("exists" | "nonempty") (Variable Specifier | MemberName)
        Rule rule = Grammar.ExistsOrNonemptyCondition;

        // TODO: Implement
    }

    @Test
    public void testExponent() {
        // Exponent: ("E"|"e") ("+"|"-")? Digits
        Rule rule = Grammar.Exponent;

        // TODO: Implement
    }

    @Test
    public void testExpression() {
        // Expression: Primary | OperatorExpression
        Rule rule = Grammar.Expression;

        // TODO: Implement
    }

    @Test
    public void testExpressionStatement() {
        // ExpressionStatement: ( Assignment | IncrementOrDecrement | Invocation ) ";"
        Rule rule = Grammar.ExpressionStatement;

        // TODO: Implement
    }

    @Test
    public void testExtendedType() {
        // ExtendedType: "extends" ("super" ".")? Type PositionalArguments
        Rule rule = Grammar.ExtendedType;

        // TODO: Implement
    }

    @Test
    public void testFail() {
        // Fail: "else" Block
        Rule rule = Grammar.Fail;

        // TODO: Implement
    }

    @Test
    public void testFinally() {
        // Finally: "finally" Block
        Rule rule = Grammar.Finally;

        // TODO: Implement
    }

    @Test
    public void testFloatLiteral() {
        // FloatLiteral: Digits ("." FractionalDigits (Exponent | Magnitude | FractionalMagnitude)? | FractionalMagnitude)
        Rule rule = Grammar.FloatLiteral;

        // TODO: Implement
    }

    @Test
    public void testFor() {
        // For: "for" "(" ForIterator ")" Block
        Rule rule = Grammar.For;

        // TODO: Implement
    }

    @Test
    public void testForFail() {
        // ForFail: For Fail?
        Rule rule = Grammar.ForFail;

        // TODO: Implement
    }

    @Test
    public void testForIterator() {
        // ForIterator: IteratorVariable "in" Expression
        Rule rule = Grammar.ForIterator;

        // TODO: Implement
    }

    @Test
    public void testFractionalDigits() {
        // FractionalDigits: Digit+ | (Digit{3} "_")+ Digit{1..3}
        Rule rule = Grammar.FractionalDigits;

        // TODO: Implement
    }

    @Test
    public void testFractionalMagnitude() {
        // FractionalMagnitude: "m" | "u" | "n" | "p" | "f"
        Rule rule = Grammar.FractionalMagnitude;

        // TODO: Implement
    }

    @Test
    public void testFullPackageName() {
        Rule rule = Grammar.FullPackageName;

        valid(rule, LIDENTIFIER);
        valid(rule, LIDENTIFIER, MEMBER_OP, LIDENTIFIER);

        invalid(rule, MEMBER_OP, LIDENTIFIER);
    }

    @Test
    public void testFunctionalArguments() {
        // FunctionalArguments: (MemberName FunctionalBody)+
        Rule rule = Grammar.FunctionalArguments;

        // TODO: Implement
    }

    @Test
    public void testFunctionalBody() {
        // FunctionalBody: Params? ( Block | "(" Expression ")" )
        Rule rule = Grammar.FunctionalBody;

        // TODO: Implement
    }

    @Test
    public void testFunctionalNamedArgument() {
        // FunctionalNamedArgument: (UnionType | "function" | "void") MemberName Params+ (Block | NamedArguments)
        Rule rule = Grammar.FunctionalNamedArgument;

        // TODO: Implement
    }

    @Test
    public void testFunctionMeta() {
        // FunctionMeta: MemberName TypeArguments?
        Rule rule = Grammar.FunctionMeta;

        // TODO: Implement
    }

    @Test
    public void testIdentifierChar() {
        // IdentifierChar: LowercaseChar | UppercaseChar | Digit
        Rule rule = Grammar.IdentifierChar;

        // TODO: Implement
    }

    @Test
    public void testIf() {
        // If: "if" "(" Condition ")" Block
        Rule rule = Grammar.If;

        // TODO: Implement
    }

    @Test
    public void testIfElse() {
        // IfElse: If Else?
        Rule rule = Grammar.IfElse;

        // TODO: Implement
    }

    @Test
    public void testImport() {
        // Import:  "import" FullPackageName "{" ImportElements? "}"
        Rule rule = Grammar.Import;

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
    public void testImportElement() {
        // ImportElement: ImportTypeElement | ImportMethodAttributeElement
        Rule rule = Grammar.ImportElement;

        // TODO: Implement
    }

    @Test
    public void testImportElements() {
        // ImportElements:  ImportElement ("," ImportElement)* ("," ImportWildcard)? | ImportWildcard
        Rule rule = Grammar.ImportElements;

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
    public void testImportMethodAttributeElement() {
        // ImportMethodAttributeElement: MethodAttributeAlias? MemberName
        Rule rule = Grammar.ImportMethodAttributeElement;

        // TODO: Implement
    }

    @Test
    public void testImportTypeElement() {
        // ImportTypeElement: TypeAlias? TypeName
        Rule rule = Grammar.ImportTypeElement;

        // TODO: Implement
    }

    @Test
    public void testImportWildcard() {
        // ImportWildcard: "..."
        Rule rule = Grammar.ImportWildcard;

        // TODO: Implement
    }

    @Test
    public void testIncrementOrDecrement() {
        // IncrementOrDecrement: "--" | "++" ;
        Rule rule = Grammar.IncrementOrDecrement;

        // TODO: Implement
    }

    @Test
    public void testInitializer() {
        // Initializer: ":=" Expression
        Rule rule = Grammar.Initializer;

        // TODO: Implement
    }

    @Test
    public void testInitializerReference() {
        // InitializerReference: (Receiver ".")? TypeName TypeArguments?
        Rule rule = Grammar.InitializerReference;

        // TODO: Implement
    }

    @Test
    public void testIntegerLiteral() {
        // IntegerLiteral: Digits Magnitude?
        Rule rule = Grammar.IntegerLiteral;

        // TODO: Implement
    }

    @Test
    public void testInterface() {
        // Interface: Annotation* InterfaceHeader (InterfaceBody | TypeSpecifier ";")
        Rule rule = Grammar.Interface;

        // TODO: Implement
    }

    @Test
    public void testInterfaceBody() {
        // InterfaceBody: "{" Declaration* "}"
        Rule rule = Grammar.InterfaceBody;

        // TODO: Implement
    }

    @Test
    public void testInterfaceHeader() {
        // InterfaceHeader: "interface" TypeName TypeParams? InterfaceInheritance TypeConstraints?
        Rule rule = Grammar.InterfaceHeader;

        // TODO: Implement
    }

    @Test
    public void testInterfaceInheritance() {
        // InterfaceInheritance: CaseTypes? Metatypes? AdaptedTypes? SatisfiedTypes?
        Rule rule = Grammar.InterfaceInheritance;

        // TODO: Implement
    }

    @Test
    public void testIntersectionType() {
        // IntersectionType: EntryType ("&" EntryType)*
        Rule rule = Grammar.IntersectionType;

        // TODO: Implement
    }

    @Test
    public void testIntroduction() {
        // Introduction: "adapt" Type SatisfiedTypes TypeConstraints? ";"
        Rule rule = Grammar.Introduction;

        // TODO: Implement
    }

    @Test
    public void testInvocation() {
        // Invocation: Primary Arguments | SequenceInstantiation
        Rule rule = Grammar.Invocation;

        // TODO: Implement
    }

    @Test
    public void testIsCondition() {
        // IsCondition: "is" (TypedVariable Specifier | UnionType MemberName)
        Rule rule = Grammar.IsCondition;

        // TODO: Implement
    }

    @Test
    public void testIteratorVariable() {
        // IteratorVariable: Variable | CallableVariable | EntryVariablePair
        Rule rule = Grammar.IteratorVariable;

        // TODO: Implement
    }

    @Test
    public void testLineComment() {
        // LineComment: ("//"|"#!") ~(Newline|Return)* (Return Newline | Return | Newline)?
        Rule rule = Grammar.LineComment;

        // TODO: Implement
    }

    @Test
    public void testLiteral() {
        // Literal: IntegerLiteral | FloatLiteral | CharacterLiteral | StringLiteral | QuotedLiteral
        Rule rule = Grammar.Literal;

        // TODO: Implement
    }

    @Test
    public void testLocalNamedArgument() {
        // LocalNamedArgument: (UnionType | "value") MemberName (Block | NamedArguments)
        Rule rule = Grammar.LocalNamedArgument;

        // TODO: Implement
    }

    @Test
    public void testLoopCondition() {
        // LoopCondition: "while" "(" Condition ")"
        Rule rule = Grammar.LoopCondition;

        // TODO: Implement
    }

    @Test
    public void testLowercaseChar() {
        // LowercaseChar: "a".."z" | "_" ;
        Rule rule = Grammar.LowercaseChar;

        // TODO: Implement
    }

    @Test
    public void testMagnitude() {
        // Magnitude: "k" | "M" | "G" | "T" | "P"
        Rule rule = Grammar.Magnitude;

        // TODO: Implement
    }

    @Test
    public void testMemberName() {
        // MemberName: LIdentifier
        Rule rule = Grammar.MemberName;

        // TODO: Implement
    }

    @Test
    public void testMemberReference() {
        // MemberReference: CallableReference | ValueReference
        Rule rule = Grammar.MemberReference;

        // TODO: Implement
    }

    @Test
    public void testMeta() {
        // Meta: TypeMeta | MethodMeta | AttributeMeta | FunctionMeta | ValueMeta
        Rule rule = Grammar.Meta;

        // TODO: Implement
    }

    @Test
    public void testMetatypes() {
        // Metatypes: "is" Type ("&" Type)*
        Rule rule = Grammar.Metatypes;

        // TODO: Implement
    }

    @Test
    public void testMethod() {
        // Method: Annotation* MethodHeader (Block | NamedArguments | Specifier? ";")
        Rule rule = Grammar.Method;

        // TODO: Implement
    }

    @Test
    public void testMethodAttributeAlias() {
        // MethodAttributeAlias: MemberName "="
        Rule rule = Grammar.MethodAttributeAlias;

        // TODO: Implement
    }

    @Test
    public void testMethodHeader() {
        // MethodHeader: (UnionType | "function" | "void") MemberName TypeParams? Params+ Metatypes? TypeConstraints?
        Rule rule = Grammar.MethodHeader;

        // TODO: Implement
    }

    @Test
    public void testMethodMeta() {
        // MethodMeta: Type "." MemberName TypeArguments?
        Rule rule = Grammar.MethodMeta;

        // TODO: Implement
    }

    @Test
    public void testMethodReference() {
        // MethodReference: (Receiver ".")? MemberName TypeArguments?
        Rule rule = Grammar.MethodReference;

        // TODO: Implement
    }

    @Test
    public void testMultilineComment() {
        // MultilineComment: "/" "*" ( MultilineCommmentCharacter | MultilineComment )* "*" "/"
        Rule rule = Grammar.MultilineComment;

        // TODO: Implement
    }

    @Test
    public void testMultilineCommmentCharacter() {
        // MultilineCommmentCharacter: ~("/"|"*") | ("/" ~"*") => "/" | ("*" ~"/") => "*"
        Rule rule = Grammar.MultilineCommmentCharacter;

        // TODO: Implement
    }

    @Test
    public void testNamedArgument() {
        // NamedArgument: SpecifiedNamedArgument | LocalNamedArgument | FunctionalNamedArgument | Object
        Rule rule = Grammar.NamedArgument;

        // TODO: Implement
    }

    @Test
    public void testNamedArguments() {
        // NamedArguments: "{" NamedArgument* Sequence? "}"
        Rule rule = Grammar.NamedArguments;

        // TODO: Implement
    }

    @Test
    public void testObject() {
        // Object: Annotation* ObjectHeader ClassBody
        Rule rule = Grammar.Object;

        // TODO: Implement
    }

    @Test
    public void testObjectHeader() {
        // ObjectHeader: "object" MemberName ObjectInheritance
        Rule rule = Grammar.ObjectHeader;

        // TODO: Implement
    }

    @Test
    public void testObjectInheritance() {
        // ObjectInheritance: ExtendedType? SatisfiedTypes?
        Rule rule = Grammar.ObjectInheritance;

        // TODO: Implement
    }

    @Test
    public void testOperatorExpression() {
        // OperatorExpression: ????? }
        // TODO: Check out language spec
        Rule rule = Grammar.OperatorExpression;

        // TODO: Implement
    }

    @Test
    public void testOuterReference() {
        // OuterReference: (Receiver ".")? "outer"
        Rule rule = Grammar.OuterReference;

        // TODO: Implement
    }

    @Test
    public void testPackageName() {
        // PackageName: PIdentifier
        Rule rule = Grammar.PackageName;

        // TODO: Implement
    }

    @Test
    public void testParam() {
        // Param: Annotation* (SimpleParam | CallableParam | EntryParamPair)
        Rule rule = Grammar.Param;

        // TODO: Implement
    }

    @Test
    public void testParams() {
        // Params: "(" Param ("," Param)* ("," DefaultParam)* ("," SequencedParam)? | DefaultParam ("," DefaultParam)* ("," SequencedParam)? | SequencedParam? ")"
        Rule rule = Grammar.Params;

        // TODO: Implement
    }

    @Test
    public void testParenDimension() {
        // ParenDimension: "(" Dimension ")"
        Rule rule = Grammar.ParenDimension;

        // TODO: Implement
    }

    @Test
    public void testParExpression() {
        // ParExpression: "(" Expression ")"
        Rule rule = Grammar.ParExpression;

        // TODO: Implement
    }

    @Test
    public void testPositionalArguments() {
        // PositionalArguments: "(" Expression ("," Expression)* ("," Sequence)? | Sequence? ")"
        Rule rule = Grammar.PositionalArguments;

        // TODO: Implement
    }

    @Test
    public void testPrimary() {
        // Primary: Atom | Meta | MemberReference | Invocation
        Rule rule = Grammar.Primary;

        // TODO: Implement
    }

    @Test
    public void testQuotedLiteral() {
        // QuotedLiteral: "'" QuotedLiteralCharacter* "'"
        Rule rule = Grammar.QuotedLiteral;

        // TODO: Implement
    }

    @Test
    public void testQuotedLiteralCharacter() {
        // QuotedLiteralCharacter: ~("'")
        Rule rule = Grammar.QuotedLiteralCharacter;

        // TODO: Implement
    }

    @Test
    public void testReceiver() {
        // Receiver: Primary
        Rule rule = Grammar.Receiver;

        // TODO: Implement
    }

    @Test
    public void testResource() {
        // Resource: MemberName | InitializerReference Arguments | Variable Specifier
        Rule rule = Grammar.Resource;

        // TODO: Implement
    }

    @Test
    public void testRetry() {
        // Retry: "retry"
        Rule rule = Grammar.Retry;

        // TODO: Implement
    }

    @Test
    public void testReturn() {
        // Return: "return" Expression?
        Rule rule = Grammar.Return;

        // TODO: Implement
    }

    @Test
    public void testSatisfiedTypes() {
        // SatisfiedTypes: "satisfies" Type ("&" Type)*
        Rule rule = Grammar.SatisfiedTypes;

        // TODO: Implement
    }

    @Test
    public void testSatisfiesCondition() {
        // SatisfiesCondition: "satisfies" Type Type
        Rule rule = Grammar.SatisfiesCondition;

        // TODO: Implement
    }

    @Test
    public void testSelfReference() {
        // SelfReference: "this" | "super" | "outer"
        Rule rule = Grammar.SelfReference;

        // TODO: Implement
    }

    @Test
    public void testSequence() {
        // Sequence: Expression ("," Expression)* | Expression "..."
        Rule rule = Grammar.Sequence;

        // TODO: Implement
    }

    @Test
    public void testSequencedParam() {
        // SequencedParam: Annotation* UnionType "..." MemberName
        Rule rule = Grammar.SequencedParam;

        // TODO: Implement
    }

    @Test
    public void testSequencedType() {
        // SequencedType: TypeName "..."
        Rule rule = Grammar.SequencedType;

        // TODO: Implement
    }

    @Test
    public void testSequencedTypeParam() {
        // SequencedTypeParam: TypeName "..."
        Rule rule = Grammar.SequencedTypeParam;

        // TODO: Implement
    }

    @Test
    public void testSequenceInstantiation() {
        // SequenceInstantiation: "{" Sequence? "}" ;
        Rule rule = Grammar.SequenceInstantiation;

        // TODO: Implement
    }

    @Test
    public void testSimpleAttribute() {
        // SimpleAttribute: AttributeHeader ( (Specifier | Initializer)? ";" | NamedArguments )
        Rule rule = Grammar.SimpleAttribute;

        // TODO: Implement
    }

    @Test
    public void testSimpleParam() {
        // SimpleParam: UnionType MemberName
        Rule rule = Grammar.SimpleParam;

        // TODO: Implement
    }

    @Test
    public void testSpecification() {
        // Specification: MemberName Specifier ";"
        Rule rule = Grammar.Specification;

        // TODO: Implement
    }

    @Test
    public void testSpecifiedNamedArgument() {
        // SpecifiedNamedArgument: MemberName Specifier ";"
        Rule rule = Grammar.SpecifiedNamedArgument;

        // TODO: Implement
    }

    @Test
    public void testSpecifier() {
        // Specifier: "=" Expression
        Rule rule = Grammar.Specifier;

        // TODO: Implement
    }

    @Test
    public void testStatement() {
        // Statement: ExpressionStatement | Specification | DirectiveStatement | ControlStructure
        Rule rule = Grammar.Statement;

        // TODO: Implement
    }

    @Test
    public void testStringCharacter() {
        // StringCharacter: ~( "\" | "\"" ) | EscapeSequence
        Rule rule = Grammar.StringCharacter;

        // TODO: Implement
    }

    @Test
    public void testStringTemplate() {
        // StringTemplate: StringLiteral (Expression StringLiteral)+
        Rule rule = Grammar.StringTemplate;

        // TODO: Implement
    }

    @Test
    public void testSubtype() {
        // Subtype: "subtype" | MemberName "." "subtype"
        Rule rule = Grammar.Subtype;

        // TODO: Implement
    }

    @Test
    public void testSwitch() {
        // Switch: "switch" "(" Expression ")"
        Rule rule = Grammar.Switch;

        // TODO: Implement
    }

    @Test
    public void testSwitchCaseElse() {
        // SwitchCaseElse: Switch ( Cases | "{" Cases "}" )
        Rule rule = Grammar.SwitchCaseElse;

        // TODO: Implement
    }

    @Test
    public void testThrow() {
        // Throw: "throw" Expression?
        Rule rule = Grammar.Throw;

        // TODO: Implement
    }

    @Test
    public void testTimeLiteral() {
        // TimeLiteral: "'" Digit{1,2} ":" Digit{2} ( ":" Digit{2} ( ":" Digit{3} )? )? (" " "AM"|"PM")? (" " Character{3,4})? "'"
        Rule rule = Grammar.TimeLiteral;

        // TODO: Implement
    }

    @Test
    public void testTry() {
        // Try: "try" ("(" Resource ")")? Block
        Rule rule = Grammar.Try;

        // TODO: Implement
    }

    @Test
    public void testTryCatchFinally() {
        // TryCatchFinally: Try Catch* Finally?
        Rule rule = Grammar.TryCatchFinally;

        // TODO: Implement
    }

    @Test
    public void testType() {
        // Type: TypeNameWithArguments ("." TypeNameWithArguments)*
        Rule rule = Grammar.Type;

        // TODO: Implement
    }

    @Test
    public void testTypeAlias() {
        // TypeAlias: TypeName "="
        Rule rule = Grammar.TypeAlias;

        // TODO: Implement
    }

    @Test
    public void testTypeArgument() {
        // TypeArgument: UnionType | Dimension
        Rule rule = Grammar.TypeArgument;

        // TODO: Implement
    }

    @Test
    public void testTypeArguments() {
        // TypeArguments: "<" (UnionType ",")* (UnionType | SequencedType) ">"
        Rule rule = Grammar.TypeArguments;

        // TODO: Implement
    }

    @Test
    public void testTypeConstraint() {
        // TypeConstraint: "given" TypeName TypeParams? Params? TypeConstraintInheritance
        Rule rule = Grammar.TypeConstraint;

        // TODO: Implement
    }

    @Test
    public void testTypeConstraintInheritance() {
        // TypeConstraintInheritance: CaseTypes? Metatypes? SatisfiedTypes? AbstractedType?
        Rule rule = Grammar.TypeConstraintInheritance;

        // TODO: Implement
    }

    @Test
    public void testTypeConstraints() {
        // TypeConstraints: TypeConstraint+
        Rule rule = Grammar.TypeConstraints;

        // TODO: Implement
    }

    @Test
    public void testTypeDeclaration() {
        // TypeDeclaration: Class | Object | Interface
        Rule rule = Grammar.TypeDeclaration;

        // TODO: Implement
    }

    @Test
    public void testTypedQuotedLiteral() {
        // TypedQuotedLiteral: TypeName QuotedLiteral
        Rule rule = Grammar.TypedQuotedLiteral;

        // TODO: Implement
    }

    @Test
    public void testTypedVariable() {
        // TypedVariable: UnionType MemberName
        Rule rule = Grammar.TypedVariable;

        // TODO: Implement
    }

    @Test
    public void testTypeMeta() {
        // TypeMeta: Type
        Rule rule = Grammar.TypeMeta;

        // TODO: Implement
    }

    @Test
    public void testTypeName() {
        // TypeName: UIdentifier
        Rule rule = Grammar.TypeName;

        // TODO: Implement
    }

    @Test
    public void testTypeNameWithArguments() {
        // TypeNameWithArguments: TypeName TypeArguments?
        Rule rule = Grammar.TypeNameWithArguments;

        // TODO: Implement
    }

    @Test
    public void testTypeParam() {
        // TypeParam: Variance? TypeName
        Rule rule = Grammar.TypeParam;

        // TODO: Implement
    }

    @Test
    public void testTypeParams() {
        // TypeParams: "<" (TypeParam ",")* (TypeParam | SequencedTypeParam) ">"
        Rule rule = Grammar.TypeParams;

        // TODO: Implement
    }

    @Test
    public void testTypeSpecifier() {
        // TypeSpecifier: "=" Type
        Rule rule = Grammar.TypeSpecifier;

        // TODO: Implement
    }

    @Test
    public void testUnionType() {
        // UnionType: IntersectionType ("|" IntersectionType)*
        Rule rule = Grammar.UnionType;

        // TODO: Implement
    }

    @Test
    public void testUppercaseChar() {
        // UppercaseChar: "A".."Z" ;
        Rule rule = Grammar.UppercaseChar;

        // TODO: Implement
    }

    @Test
    public void testValueMeta() {
        // ValueMeta: MemberName TypeArguments?
        Rule rule = Grammar.ValueMeta;

        // TODO: Implement
    }

    @Test
    public void testValueReference() {
        // ValueReference: (Receiver ".")? MemberName
        Rule rule = Grammar.ValueReference;

        // TODO: Implement
    }

    @Test
    public void testVariable() {
        // Variable: UnionType? MemberName
        Rule rule = Grammar.Variable;

        // TODO: Implement
    }

    @Test
    public void testVariance() {
        // Variance: "out" | "in"
        Rule rule = Grammar.Variance;

        // TODO: Implement
    }

    @Test
    public void testWhile() {
        // While: LoopCondition Block
        Rule rule = Grammar.While;

        // TODO: Implement
    }

    @Test
    public void testWhitespace() {
        // Whitespace: " " | Tab | Formfeed | Newline | Return
        Rule rule = Grammar.Whitespace;

        // TODO: Implement
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
