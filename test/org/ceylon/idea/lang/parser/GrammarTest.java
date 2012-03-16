package org.ceylon.idea.lang.parser;

import org.ceylon.idea.lang.lexer.CeylonToken;
import org.ceylon.idea.lang.parser.rule.MockPsiBuilder;
import org.ceylon.idea.lang.parser.rule.Rule;
import org.junit.Ignore;
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
        valid(rule, UIDENTIFIER, MEMBER_OP, UIDENTIFIER, QMARK, ARRAY);
    }

    @Test
    public void testAbstractedType() {
        // AbstractedType: "abstracts" Type
        Rule rule = Grammar.AbstractedType;

        valid(rule, ABSTRACTED_TYPE, UIDENTIFIER);
        valid(rule, ABSTRACTED_TYPE, UIDENTIFIER, MEMBER_OP, UIDENTIFIER);
    }

    @Test
    public void testAdaptedTypes() {
        // AdaptedTypes: "adapts" Type ("&" Type)*
        Rule rule = Grammar.AdaptedTypes;

        valid(rule, ADAPTED_TYPES, UIDENTIFIER);
        valid(rule, ADAPTED_TYPES, UIDENTIFIER, INTERSECTION_OP, UIDENTIFIER);
        valid(rule, ADAPTED_TYPES, UIDENTIFIER, INTERSECTION_OP, UIDENTIFIER, INTERSECTION_OP, UIDENTIFIER);
        valid(rule, ADAPTED_TYPES, UIDENTIFIER, MEMBER_OP, UIDENTIFIER, INTERSECTION_OP, UIDENTIFIER);
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
    public void testArguments() {
        // Arguments: PositionalArguments FunctionalArguments? | NamedArguments
        Rule rule = Grammar.Arguments;

        valid(rule, LPAREN, STRING_LITERAL, RPAREN);
        valid(rule, LBRACE, LIDENTIFIER, SPECIFY, STRING_LITERAL, SEMICOLON, RBRACE);

        // TODO: FunctionalArguments
    }

    @Test
    @Ignore
    public void testAssignment() {
        // Assignment: ":=" | ".=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^="| "~=" | "&&=" | "||=" ;
        Rule rule = Grammar.Assignment;

        valid(rule, ASSIGN_OP);
    }

    @Test
    @Ignore
    public void testAtom() {
        // Atom: Literal | StringTemplate | SelfReference | ParExpression
        Rule rule = Grammar.Atom;

        // TODO: Implement
    }

    @Test
    public void testAttribute() {
        // Attribute: Annotation* (SimpleAttribute | AttributeGetter | AttributeSetter)
        Rule rule = Grammar.Attribute;

        valid(rule, VALUE_MODIFIER, LIDENTIFIER, SEMICOLON);
        valid(rule, VALUE_MODIFIER, LIDENTIFIER, LBRACE, RBRACE);
        valid(rule, ASSIGN, LIDENTIFIER, LBRACE, RBRACE);
        valid(rule, LIDENTIFIER, LIDENTIFIER, VALUE_MODIFIER, LIDENTIFIER, SEMICOLON);
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

        valid(rule, UIDENTIFIER, LIDENTIFIER);
        valid(rule, UIDENTIFIER, UNION_OP, UIDENTIFIER, LIDENTIFIER);
        valid(rule, VALUE_MODIFIER, LIDENTIFIER);
    }

    @Test
    @Ignore
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

        valid(rule, LBRACE, RBRACE);
        valid(rule, LBRACE, UIDENTIFIER, LIDENTIFIER, SEMICOLON, RBRACE);
        valid(rule, LBRACE, RETURN, SEMICOLON, RBRACE);
    }

    @Test
    @Ignore
    public void testBooleanCondition() {
        // BooleanCondition: Expression
        Rule rule = Grammar.BooleanCondition;

        // TODO: Implement
    }

    @Test
    public void testCallableParam() {
        // CallableParam: (UnionType | "void") MemberName Params+
        Rule rule = Grammar.CallableParam;

        valid(rule, UIDENTIFIER, LIDENTIFIER, LPAREN, RPAREN);
        valid(rule, VOID_MODIFIER, LIDENTIFIER, LPAREN, RPAREN, LPAREN, RPAREN);
    }

    @Test
    public void testCallableReference() {
        // CallableReference: MethodReference | InitializerReference
        Rule rule = Grammar.CallableReference;

        valid(rule, LIDENTIFIER);
        valid(rule, LIDENTIFIER, SMALLER_OP, UIDENTIFIER, LARGER_OP);
        valid(rule, UIDENTIFIER);
        valid(rule, UIDENTIFIER, SMALLER_OP, UIDENTIFIER, LARGER_OP);
    }

    @Test
    public void testCallableVariable() {
        // CallableVariable: (UnionType | "void")? MemberName Params+
        Rule rule = Grammar.CallableVariable;

        valid(rule, LIDENTIFIER, LPAREN, RPAREN);
        valid(rule, UIDENTIFIER, LIDENTIFIER, LPAREN, RPAREN);
        valid(rule, UIDENTIFIER, UNION_OP, UIDENTIFIER, LIDENTIFIER, LPAREN, RPAREN);
        valid(rule, VOID_MODIFIER, LIDENTIFIER, LPAREN, RPAREN);
    }

    @Test
    public void testCase() {
        // Case: Expression ("," Expression)* | "is" UnionType | "satisfies" Type
        Rule rule = Grammar.Case;

        valid(rule, CHAR_LITERAL);
        valid(rule, CHAR_LITERAL, COMMA, CHAR_LITERAL);
        valid(rule, IS_OP, UIDENTIFIER);
        valid(rule, IS_OP, UIDENTIFIER, UNION_OP, UIDENTIFIER);
        valid(rule, SATISFIES, UIDENTIFIER);
        valid(rule, SATISFIES, UIDENTIFIER, MEMBER_OP, UIDENTIFIER);
    }

    @Test
    public void testCaseItem() {
        // CaseItem: "case" "(" Case ")" Block
        Rule rule = Grammar.CaseItem;

        valid(rule, CASE_CLAUSE, LPAREN, CHAR_LITERAL, RPAREN, LBRACE, RBRACE);
    }

    @Test
    public void testCases() {
        // Cases: CaseItem+ DefaultCaseItem?
        Rule rule = Grammar.Cases;

        valid(rule, CASE_CLAUSE, LPAREN, CHAR_LITERAL, RPAREN, LBRACE, RBRACE);
        valid(rule, CASE_CLAUSE, LPAREN, CHAR_LITERAL, RPAREN, LBRACE, RBRACE, CASE_CLAUSE, LPAREN, CHAR_LITERAL, RPAREN, LBRACE, RBRACE);
        valid(rule, CASE_CLAUSE, LPAREN, CHAR_LITERAL, RPAREN, LBRACE, RBRACE, ELSE_CLAUSE, LBRACE, RBRACE);
    }

    @Test
    public void testCaseType() {
        // CaseType: MemberName | Type
        Rule rule = Grammar.CaseType;

        valid(rule, LIDENTIFIER);
        valid(rule, UIDENTIFIER);
    }

    @Test
    public void testCaseTypes() {
        // CaseTypes: "of" CaseType ("|" CaseType)*
        Rule rule = Grammar.CaseTypes;

        valid(rule, CASE_TYPES, UIDENTIFIER);
        valid(rule, CASE_TYPES, UIDENTIFIER, UNION_OP, UIDENTIFIER);
        valid(rule, CASE_TYPES, UIDENTIFIER, UNION_OP, LIDENTIFIER);
        valid(rule, CASE_TYPES, LIDENTIFIER, UNION_OP, UIDENTIFIER);
        valid(rule, CASE_TYPES, LIDENTIFIER, UNION_OP, UIDENTIFIER, MEMBER_OP, UIDENTIFIER);
    }

    @Test
    public void testCatch() {
        // Catch: "catch" "(" Variable ")" Block
        Rule rule = Grammar.Catch;

        valid(rule, CATCH_CLAUSE, LPAREN, LIDENTIFIER, RPAREN, LBRACE, RBRACE);
        valid(rule, CATCH_CLAUSE, LPAREN, UIDENTIFIER, LIDENTIFIER, RPAREN, LBRACE, RBRACE);
        valid(rule, CATCH_CLAUSE, LPAREN, UIDENTIFIER, UNION_OP, UIDENTIFIER, LIDENTIFIER, RPAREN, LBRACE, RBRACE);
    }

    @Test
    public void testClass() {
        // Class: Annotation* ClassHeader (ClassBody | TypeSpecifier ";")
        Rule rule = Grammar.Class;

        valid(rule, CLASS_DEFINITION, UIDENTIFIER, LPAREN, RPAREN, LBRACE, RBRACE);
        valid(rule, CLASS_DEFINITION, UIDENTIFIER, LPAREN, RPAREN, SPECIFY, UIDENTIFIER, SEMICOLON);
        valid(rule, LIDENTIFIER, LIDENTIFIER, CLASS_DEFINITION, UIDENTIFIER, LPAREN, RPAREN, LBRACE, RBRACE);
    }

    @Test
    public void testClassBody() {
        // ClassBody: "{" (Declaration | Statement)* "}"
        Rule rule = Grammar.ClassBody;

        valid(rule, LBRACE, RBRACE);
        valid(rule, LBRACE, UIDENTIFIER, LIDENTIFIER, SEMICOLON, RBRACE);
        valid(rule, LBRACE, RETURN, SEMICOLON, RBRACE);
    }

    @Test
    public void testClassHeader() {
        // ClassHeader: "class" TypeName TypeParams? Params ClassInheritance TypeConstraints?
        Rule rule = Grammar.ClassHeader;

        valid(rule, CLASS_DEFINITION, UIDENTIFIER, LPAREN, RPAREN);
        valid(rule, CLASS_DEFINITION, UIDENTIFIER, LPAREN, RPAREN, CASE_TYPES, UIDENTIFIER);
        valid(rule, CLASS_DEFINITION, UIDENTIFIER, SMALLER_OP, UIDENTIFIER, LARGER_OP, LPAREN, RPAREN);
        valid(rule, CLASS_DEFINITION, UIDENTIFIER, SMALLER_OP, UIDENTIFIER, COMMA, UIDENTIFIER, LARGER_OP, LPAREN, RPAREN);
        valid(rule, CLASS_DEFINITION, UIDENTIFIER, LPAREN, RPAREN, TYPE_CONSTRAINT, UIDENTIFIER);
        valid(rule, CLASS_DEFINITION, UIDENTIFIER, LPAREN, RPAREN, TYPE_CONSTRAINT, UIDENTIFIER, TYPE_CONSTRAINT, UIDENTIFIER);
    }

    @Test
    public void testClassInheritance() {
        // ClassInheritance: CaseTypes? Metatypes? ExtendedType? SatisfiedTypes?
        Rule rule = Grammar.ClassInheritance;

        valid(rule, CASE_TYPES, UIDENTIFIER);
        valid(rule, IS_OP, UIDENTIFIER);
        valid(rule, EXTENDS, UIDENTIFIER, LPAREN, RPAREN);
        valid(rule, SATISFIES, UIDENTIFIER);
        valid(rule, CASE_TYPES, UIDENTIFIER, IS_OP, UIDENTIFIER, EXTENDS, UIDENTIFIER, LPAREN, RPAREN, SATISFIES, UIDENTIFIER);
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

        valid(rule, COMPILER_ANNOTATION, LIDENTIFIER);
        valid(rule, COMPILER_ANNOTATION, LIDENTIFIER, INDEX_OP, STRING_LITERAL, RBRACKET);
    }

    @Test
    public void testCompilerAnnotations() {
        // CompilerAnnotations: CompilerAnnotation*
        Rule rule = Grammar.CompilerAnnotations;

        valid(rule);
        valid(rule, COMPILER_ANNOTATION, LIDENTIFIER, COMPILER_ANNOTATION, LIDENTIFIER);
        valid(rule, COMPILER_ANNOTATION, LIDENTIFIER, INDEX_OP, STRING_LITERAL, RBRACKET, COMPILER_ANNOTATION, LIDENTIFIER);
    }

    @Test
    @Ignore
    public void testCondition() {
        // Condition: BooleanCondition | IsCondition | ExistsOrNonemptyCondition | SatisfiesCondition
        Rule rule = Grammar.Condition;

        // TODO: Implement
    }

    @Test
    @Ignore
    public void testDateLiteral() {
        // DateLiteral: "'" Digit{1,2} "/" Digit{1,2} "/" Digit{4} "'"
        Rule rule = Grammar.DateLiteral;

        // TODO: Implement
    }

    @Test
    public void testDeclaration() {
        // Declaration: Method | Attribute | TypeDeclaration
        Rule rule = Grammar.Declaration;

        valid(rule, VOID_MODIFIER, LIDENTIFIER, LPAREN, RPAREN, LBRACE, RBRACE);
        valid(rule, VALUE_MODIFIER, LIDENTIFIER, SEMICOLON);
        // TODO: TypeDeclaration
    }

    @Test
    public void testDefaultCaseItem() {
        // DefaultCaseItem: "else" Block
        Rule rule = Grammar.DefaultCaseItem;

        valid(rule, ELSE_CLAUSE, LBRACE, RBRACE);
    }

    @Test
    public void testDefaultParam() {
        // DefaultParam: Param Specifier
        Rule rule = Grammar.DefaultParam;

        valid(rule, UIDENTIFIER, LIDENTIFIER, SPECIFY, STRING_LITERAL);
    }


    @Test
    public void testDirectiveStatement() {
        // DirectiveStatement: Directive ";"
        Rule rule = Grammar.DirectiveStatement;

        valid(rule, RETURN, SEMICOLON);
        valid(rule, THROW, SEMICOLON);
        valid(rule, BREAK, SEMICOLON);
        valid(rule, CONTINUE, SEMICOLON);
    }

    @Test
    public void testEntryParamPair() {
        // EntryParamPair: SimpleParam "->" SimpleParam
        Rule rule = Grammar.EntryParamPair;

        valid(rule, UIDENTIFIER, LIDENTIFIER, ENTRY_OP, UIDENTIFIER, LIDENTIFIER);
    }

    @Test
    @Ignore
    public void testEntryType() {
        // EntryType: AbbreviatedType ("->" AbbreviatedType)?
        Rule rule = Grammar.EntryType;

        // TODO: Implement
    }

    @Test
    @Ignore
    public void testEntryVariablePair() {
        // EntryVariablePair: Variable "->" Variable
        Rule rule = Grammar.EntryVariablePair;

        // TODO: Implement
    }

    @Test
    public void testExistsOrNonemptyCondition() {
        // ExistsOrNonemptyCondition: ("exists" | "nonempty") (Variable Specifier | MemberName)
        Rule rule = Grammar.ExistsOrNonemptyCondition;

        valid(rule, EXISTS, LIDENTIFIER);
        valid(rule, NONEMPTY, LIDENTIFIER);
        valid(rule, EXISTS, LIDENTIFIER, SPECIFY, STRING_LITERAL);
    }

    @Test
    @Ignore
    public void testExponent() {
        // Exponent: ("E"|"e") ("+"|"-")? Digits
        Rule rule = Grammar.Exponent;

        // TODO: Implement
    }

    @Test
    @Ignore
    public void testExpression() {
        // Expression: Primary | OperatorExpression
        Rule rule = Grammar.Expression;

        // TODO: Implement
    }

    @Test
    public void testExpressionStatement() {
        // ExpressionStatement: ( Assignment | IncrementOrDecrement | Invocation ) ";"
        Rule rule = Grammar.ExpressionStatement;

        valid(rule, ASSIGN_OP, SEMICOLON);
        valid(rule, APPLY_OP, SEMICOLON);
        valid(rule, ADD_ASSIGN_OP, SEMICOLON);
        valid(rule, SUBTRACT_ASSIGN_OP, SEMICOLON);
        valid(rule, MULTIPLY_ASSIGN_OP, SEMICOLON);
        valid(rule, DIVIDE_ASSIGN_OP, SEMICOLON);
        valid(rule, REMAINDER_ASSIGN_OP, SEMICOLON);
        valid(rule, INTERSECT_ASSIGN_OP, SEMICOLON);
        valid(rule, UNION_ASSIGN_OP, SEMICOLON);
        valid(rule, XOR_ASSIGN_OP, SEMICOLON);
        valid(rule, COMPLEMENT_ASSIGN_OP, SEMICOLON);
        valid(rule, AND_ASSIGN_OP, SEMICOLON);
        valid(rule, OR_ASSIGN_OP, SEMICOLON);
        valid(rule, INCREMENT_OP, SEMICOLON);
        valid(rule, DECREMENT_OP, SEMICOLON);
    }

    @Test
    public void testExtendedType() {
        // ExtendedType: "extends" ("super" ".")? Type PositionalArguments
        Rule rule = Grammar.ExtendedType;

        valid(rule, EXTENDS, UIDENTIFIER, LPAREN, RPAREN);
        // TODO: more tests
    }

    @Test
    public void testFail() {
        // Fail: "else" Block
        Rule rule = Grammar.Fail;

        valid(rule, ELSE_CLAUSE, LBRACE, RBRACE);
    }

    @Test
    public void testFinally() {
        // Finally: "finally" Block
        Rule rule = Grammar.Finally;

        valid(rule, FINALLY_CLAUSE, LBRACE, RBRACE);
    }

    @Test
    @Ignore
    public void testFloatLiteral() {
        // FloatLiteral: Digits ("." FractionalDigits (Exponent | Magnitude | FractionalMagnitude)? | FractionalMagnitude)
        Rule rule = Grammar.FloatLiteral;

        // TODO: Implement
    }

    @Test
    public void testFor() {
        // For: "for" "(" ForIterator ")" Block
        Rule rule = Grammar.For;

        valid(rule, FOR_CLAUSE, LPAREN, LIDENTIFIER, IN_OP, LIDENTIFIER, RPAREN, LBRACE, RBRACE);
    }

    @Test
    public void testForFail() {
        // ForFail: For Fail?
        Rule rule = Grammar.ForFail;

        valid(rule, FOR_CLAUSE, LPAREN, LIDENTIFIER, IN_OP, LIDENTIFIER, RPAREN, LBRACE, RBRACE);
        valid(rule, FOR_CLAUSE, LPAREN, LIDENTIFIER, IN_OP, LIDENTIFIER, RPAREN, LBRACE, RBRACE, ELSE_CLAUSE, LBRACE, RBRACE);
    }

    @Test
    public void testForIterator() {
        // ForIterator: IteratorVariable "in" Expression
        Rule rule = Grammar.ForIterator;

        valid(rule, LIDENTIFIER, IN_OP, CHAR_LITERAL);
    }

    @Test
    @Ignore
    public void testFractionalDigits() {
        // FractionalDigits: Digit+ | (Digit{3} "_")+ Digit{1..3}
        Rule rule = Grammar.FractionalDigits;

        // TODO: Implement
    }

    @Test
    @Ignore
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
    @Ignore
    public void testFunctionalArguments() {
        // FunctionalArguments: (MemberName FunctionalBody)+
        Rule rule = Grammar.FunctionalArguments;

        // TODO: implement
    }

    @Test
    @Ignore
    public void testFunctionalBody() {
        // FunctionalBody: Params? ( Block | "(" Expression ")" )
        Rule rule = Grammar.FunctionalBody;

        valid(rule, LBRACE, RBRACE);
        valid(rule, LPAREN, STRING_LITERAL, RPAREN);
    }

    @Test
    public void testFunctionalNamedArgument() {
        // FunctionalNamedArgument: (UnionType | "function" | "void") MemberName Params+ (Block | NamedArguments)
        Rule rule = Grammar.FunctionalNamedArgument;

        // String|Number myfunc (String str) {}
        valid(rule, FUNCTION_MODIFIER, LIDENTIFIER, LPAREN, UIDENTIFIER, LIDENTIFIER, RPAREN, LBRACE, RBRACE);

        // function myfunc (String str) {}
        valid(rule, FUNCTION_MODIFIER, LIDENTIFIER, LPAREN, UIDENTIFIER, LIDENTIFIER, RPAREN, LBRACE, RBRACE);

        // void myfunc (String str) {str="text";}
        valid(rule, VOID_MODIFIER, LIDENTIFIER, LPAREN, UIDENTIFIER, LIDENTIFIER, RPAREN, LBRACE, LIDENTIFIER, SPECIFY, STRING_LITERAL,
                SEMICOLON, RBRACE);
    }

    @Test
    public void testFunctionMeta() {
        // FunctionMeta: MemberName TypeArguments?
        Rule rule = Grammar.FunctionMeta;

        valid(rule, LIDENTIFIER);
        valid(rule, LIDENTIFIER, SMALLER_OP, UIDENTIFIER, LARGER_OP);
    }

    @Test
    public void testIf() {
        // If: "if" "(" Condition ")" Block
        Rule rule = Grammar.If;

        valid(rule, IF_CLAUSE, LPAREN, STRING_LITERAL, RPAREN, LBRACE, RBRACE);
        valid(rule, IF_CLAUSE, LPAREN, LIDENTIFIER, RPAREN, LBRACE, RBRACE);
    }

    @Test
    public void testIfElse() {
        // IfElse: If Else?
        Rule rule = Grammar.IfElse;

        // if (this) {}
        valid(rule, IF_CLAUSE, LPAREN, THIS, RPAREN, LBRACE, RBRACE);
        // if (this) {} else {}
        valid(rule, IF_CLAUSE, LPAREN, THIS, RPAREN, LBRACE, RBRACE, ELSE_CLAUSE, LBRACE, RBRACE);
        // if (this) {} else if (this) {}
        valid(rule, IF_CLAUSE, LPAREN, THIS, RPAREN, LBRACE, RBRACE, ELSE_CLAUSE, IF_CLAUSE, LPAREN, THIS, RPAREN, LBRACE, RBRACE);
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

        valid(rule, UIDENTIFIER);
        valid(rule, LIDENTIFIER);
        valid(rule, UIDENTIFIER, SPECIFY, UIDENTIFIER);
        valid(rule, LIDENTIFIER, SPECIFY, LIDENTIFIER);

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

        valid(rule, LIDENTIFIER);
        valid(rule, LIDENTIFIER, SPECIFY, LIDENTIFIER);
    }

    @Test
    public void testImportTypeElement() {
        // ImportTypeElement: TypeAlias? TypeName
        Rule rule = Grammar.ImportTypeElement;

        valid(rule, UIDENTIFIER);
        valid(rule, UIDENTIFIER, SPECIFY, UIDENTIFIER);
    }

    @Test
    public void testInitializer() {
        // Initializer: ":=" Expression
        Rule rule = Grammar.Initializer;

        valid(rule, ASSIGN_OP, QUOTED_LITERAL);
    }

    @Test
    public void testInitializerReference() {
        // InitializerReference: (Receiver ".")? TypeName TypeArguments?
        Rule rule = Grammar.InitializerReference;

        valid(rule, UIDENTIFIER);
        valid(rule, UIDENTIFIER, SMALLER_OP, UIDENTIFIER, LARGER_OP);
        // TODO: Receiver
    }

    @Test
    @Ignore
    public void testIntegerLiteral() {
        // IntegerLiteral: Digits Magnitude?
        Rule rule = Grammar.IntegerLiteral;

        // TODO: Implement
    }

    @Test
    public void testInterface() {
        // Interface: Annotation* InterfaceHeader (InterfaceBody | TypeSpecifier ";")
        Rule rule = Grammar.Interface;

        valid(rule, INTERFACE_DEFINITION, UIDENTIFIER, LBRACE, RBRACE);
        valid(rule, INTERFACE_DEFINITION, UIDENTIFIER, SPECIFY, UIDENTIFIER, SEMICOLON);
        valid(rule, LIDENTIFIER, LIDENTIFIER, INTERFACE_DEFINITION, UIDENTIFIER, LBRACE, RBRACE);
    }

    @Test
    public void testInterfaceBody() {
        // InterfaceBody: "{" Declaration* "}"
        Rule rule = Grammar.InterfaceBody;

        valid(rule, LBRACE, RBRACE);
        valid(rule, LBRACE, UIDENTIFIER, LIDENTIFIER, SEMICOLON, VALUE_MODIFIER, LIDENTIFIER, SEMICOLON, RBRACE);

        invalid(rule, LBRACE, RETURN, SEMICOLON, RBRACE);
    }

    @Test
    public void testInterfaceHeader() {
        // InterfaceHeader: "interface" TypeName TypeParams? InterfaceInheritance TypeConstraints?
        Rule rule = Grammar.InterfaceHeader;

        valid(rule, INTERFACE_DEFINITION, UIDENTIFIER);
        valid(rule, INTERFACE_DEFINITION, UIDENTIFIER, ADAPTED_TYPES, UIDENTIFIER);
        valid(rule, INTERFACE_DEFINITION, UIDENTIFIER, SMALLER_OP, UIDENTIFIER, LARGER_OP);
        valid(rule, INTERFACE_DEFINITION, UIDENTIFIER, SMALLER_OP, UIDENTIFIER, COMMA, UIDENTIFIER, LARGER_OP);
        valid(rule, INTERFACE_DEFINITION, UIDENTIFIER, TYPE_CONSTRAINT, UIDENTIFIER);
        valid(rule, INTERFACE_DEFINITION, UIDENTIFIER, TYPE_CONSTRAINT, UIDENTIFIER, TYPE_CONSTRAINT, UIDENTIFIER);

    }

    @Test
    public void testInterfaceInheritance() {
        // InterfaceInheritance: CaseTypes? Metatypes? AdaptedTypes? SatisfiedTypes?
        Rule rule = Grammar.InterfaceInheritance;

        valid(rule, CASE_TYPES, UIDENTIFIER);
        valid(rule, IS_OP, UIDENTIFIER);
        valid(rule, ADAPTED_TYPES, UIDENTIFIER);
        valid(rule, SATISFIES, UIDENTIFIER);
        valid(rule, CASE_TYPES, UIDENTIFIER, IS_OP, UIDENTIFIER, ADAPTED_TYPES, UIDENTIFIER, SATISFIES, UIDENTIFIER);

    }

    @Test
    @Ignore
    public void testIntersectionType() {
        // IntersectionType: EntryType ("&" EntryType)*
        Rule rule = Grammar.IntersectionType;

        // TODO: Implement
    }

    @Test
    @Ignore
    public void testInvocation() {
        // Invocation: Primary Arguments | SequenceInstantiation
        Rule rule = Grammar.Invocation;

        // TODO: Implement
    }

    @Test
    public void testIsCondition() {
        // IsCondition: "is" (TypedVariable Specifier | UnionType MemberName)
        Rule rule = Grammar.IsCondition;

        valid(rule, IS_OP, UIDENTIFIER, LIDENTIFIER, SPECIFY, STRING_LITERAL);
        valid(rule, IS_OP, UIDENTIFIER, LIDENTIFIER);
        valid(rule, IS_OP, UIDENTIFIER, UNION_OP, UIDENTIFIER, LIDENTIFIER);
    }

    @Test
    public void testIteratorVariable() {
        // IteratorVariable: Variable | CallableVariable | EntryVariablePair
        Rule rule = Grammar.IteratorVariable;

        valid(rule, LIDENTIFIER);
        valid(rule, UIDENTIFIER, LIDENTIFIER);
        valid(rule, VOID_MODIFIER, LIDENTIFIER, LPAREN, RPAREN);
        valid(rule, LIDENTIFIER, ENTRY_OP, LIDENTIFIER);
    }

    @Test
    public void testLocalNamedArgument() {
        // LocalNamedArgument: (UnionType | "value") MemberName (Block | NamedArguments)
        Rule rule = Grammar.LocalNamedArgument;

        valid(rule, VALUE_MODIFIER, LIDENTIFIER, LBRACE, RBRACE);
        valid(rule, VALUE_MODIFIER, LIDENTIFIER, LBRACE, LIDENTIFIER, SPECIFY, STRING_LITERAL, SEMICOLON, RBRACE);
        valid(rule, VALUE_MODIFIER, LIDENTIFIER, LBRACE, LIDENTIFIER, SPECIFY, STRING_LITERAL, SEMICOLON, RBRACE);
        valid(rule, UIDENTIFIER, INTERSECTION_OP, UIDENTIFIER, LIDENTIFIER, LBRACE, LIDENTIFIER, SPECIFY, STRING_LITERAL, SEMICOLON,
                RBRACE);
    }

    @Test
    public void testLoopCondition() {
        // LoopCondition: "while" "(" Condition ")"
        Rule rule = Grammar.LoopCondition;

        valid(rule, WHILE_CLAUSE, LPAREN, CHAR_LITERAL, RPAREN);
        valid(rule, WHILE_CLAUSE, LPAREN, LIDENTIFIER, RPAREN);
    }

    @Test
    @Ignore
    public void testMagnitude() {
        // Magnitude: "k" | "M" | "G" | "T" | "P"
        Rule rule = Grammar.Magnitude;

        // TODO: Implement
    }

    @Test
    @Ignore
    public void testMemberName() {
        // MemberName: LIdentifier
        Rule rule = Grammar.MemberName;

        // TODO: Implement
    }

    @Test
    @Ignore
    public void testMemberReference() {
        // MemberReference: CallableReference | ValueReference
        Rule rule = Grammar.MemberReference;

        // TODO: Implement
    }

    @Test
    public void testMetatypes() {
        // Metatypes: "is" Type ("&" Type)*
        Rule rule = Grammar.Metatypes;

        valid(rule, IS_OP, UIDENTIFIER);
        valid(rule, IS_OP, UIDENTIFIER, INTERSECTION_OP, UIDENTIFIER);
        valid(rule, IS_OP, UIDENTIFIER, INTERSECTION_OP, UIDENTIFIER, MEMBER_OP, UIDENTIFIER);
    }

    @Test
    public void testMethod() {
        // Method: Annotation* MethodHeader (Block | NamedArguments | Specifier? ";")
        Rule rule = Grammar.Method;

        valid(rule, LIDENTIFIER, UIDENTIFIER, LIDENTIFIER, LPAREN, RPAREN, LBRACE, RBRACE);
        valid(rule, VOID_MODIFIER, LIDENTIFIER, LPAREN, RPAREN, LBRACE, RBRACE);
        valid(rule, VOID_MODIFIER, LIDENTIFIER, LPAREN, RPAREN, LBRACE, STRING_LITERAL, COMMA, STRING_LITERAL, RBRACE);
        valid(rule, VOID_MODIFIER, LIDENTIFIER, LPAREN, RPAREN, SEMICOLON);
        valid(rule, VOID_MODIFIER, LIDENTIFIER, LPAREN, RPAREN, SPECIFY, CHAR_LITERAL, SEMICOLON);
    }

    @Test
    public void testMethodAttributeAlias() {
        // MethodAttributeAlias: MemberName "="
        Rule rule = Grammar.MethodAttributeAlias;

        valid(rule, LIDENTIFIER, SPECIFY);
    }

    @Test
    public void testMethodHeader() {
        // MethodHeader: (UnionType | "function" | "void") MemberName TypeParams? Params+ Metatypes? TypeConstraints?
        Rule rule = Grammar.MethodHeader;

        valid(rule, UIDENTIFIER, LIDENTIFIER, LPAREN, RPAREN);
        valid(rule, FUNCTION_MODIFIER, LIDENTIFIER, LPAREN, RPAREN);
        valid(rule, VOID_MODIFIER, LIDENTIFIER, LPAREN, RPAREN);
        valid(rule, VOID_MODIFIER, LIDENTIFIER, LPAREN, UIDENTIFIER, LIDENTIFIER, RPAREN);
        // TODO: More tests
    }

    @Test
    public void testMethodMeta() {
        // MethodMeta: Type "." MemberName TypeArguments?
        Rule rule = Grammar.MethodMeta;

        valid(rule, UIDENTIFIER, MEMBER_OP, LIDENTIFIER);
        valid(rule, UIDENTIFIER, MEMBER_OP, LIDENTIFIER, SMALLER_OP, UIDENTIFIER, LARGER_OP);
    }

    @Test
    public void testMethodReference() {
        // MethodReference: (Receiver ".")? MemberName TypeArguments?
        Rule rule = Grammar.MethodReference;

        valid(rule, LIDENTIFIER);
        valid(rule, LIDENTIFIER, SMALLER_OP, UIDENTIFIER, LARGER_OP);
        // TODO Receiver
    }

    @Test
    @Ignore
    public void testNamedArgument() {
        // NamedArgument: SpecifiedNamedArgument | LocalNamedArgument | FunctionalNamedArgument | Object
        Rule rule = Grammar.NamedArgument;

        // TODO: Implement
    }

    @Test
    @Ignore
    public void testNamedArguments() {
        // NamedArguments: "{" NamedArgument* Sequence? "}"
        Rule rule = Grammar.NamedArguments;

        valid(rule, LBRACE, LIDENTIFIER, SPECIFY, STRING_LITERAL, SEMICOLON, RBRACE);
        // TODO: Implement
    }

    @Test
    public void testObject() {
        // Object: Annotation* ObjectHeader ClassBody
        Rule rule = Grammar.Object;

        valid(rule, OBJECT_DEFINITION, LIDENTIFIER, LBRACE, RBRACE);
        valid(rule, LIDENTIFIER, LIDENTIFIER, OBJECT_DEFINITION, LIDENTIFIER, LBRACE, RBRACE);
    }

    @Test
    public void testObjectHeader() {
        // ObjectHeader: "object" MemberName ObjectInheritance
        Rule rule = Grammar.ObjectHeader;

        valid(rule, OBJECT_DEFINITION, LIDENTIFIER);
        valid(rule, OBJECT_DEFINITION, LIDENTIFIER, EXTENDS, UIDENTIFIER, LPAREN, RPAREN);
        valid(rule, OBJECT_DEFINITION, LIDENTIFIER, SATISFIES, UIDENTIFIER);
    }

    @Test
    public void testObjectInheritance() {
        // ObjectInheritance: ExtendedType? SatisfiedTypes?
        Rule rule = Grammar.ObjectInheritance;

        valid(rule);
        valid(rule, EXTENDS, UIDENTIFIER, LPAREN, RPAREN);
        valid(rule, SATISFIES, UIDENTIFIER);
        valid(rule, EXTENDS, UIDENTIFIER, LPAREN, RPAREN, SATISFIES, UIDENTIFIER);
    }

    @Test
    @Ignore
    public void testOperatorExpression() {
        // OperatorExpression: ????? }
        // TODO: Check out language spec
        Rule rule = Grammar.OperatorExpression;

        // TODO: Implement
    }

    @Test
    @Ignore
    public void testParam() {
        // Param: Annotation* (SimpleParam | CallableParam | EntryParamPair)
        Rule rule = Grammar.Param;

        // TODO: Implement
    }

    @Test
    @Ignore
    public void testParams() {
        // Params: "(" Param ("," Param)* ("," DefaultParam)* ("," SequencedParam)? | DefaultParam ("," DefaultParam)* (",
        // " SequencedParam)? | SequencedParam? ")"
        Rule rule = Grammar.Params;

        // TODO: Implement
    }

    @Test
    @Ignore
    public void testParExpression() {
        // ParExpression: "(" Expression ")"
        Rule rule = Grammar.ParExpression;

        // TODO: Implement
    }

    @Test
    public void testPositionalArguments() {
        // TODO: The grammar is not clear here:
        // PositionalArguments: "(" Expression ("," Expression)* ("," Sequence)? | Sequence? ")"
        Rule rule = Grammar.PositionalArguments;

        valid(rule, LPAREN, STRING_LITERAL, RPAREN);
        valid(rule, LPAREN, STRING_LITERAL, COMMA, STRING_LITERAL, RPAREN);
        // TODO: more tests after grammar clarification
    }

    @Test
    @Ignore
    public void testPrimary() {
        // Primary: Atom | Meta | MemberReference | Invocation
        Rule rule = Grammar.Primary;

        // TODO: Implement
    }

    @Test
    @Ignore
    public void testReceiver() {
        // Receiver: Primary
        Rule rule = Grammar.Receiver;

        // TODO: Implement
    }

    @Test
    public void testResource() {
        // Resource: MemberName | InitializerReference Arguments | Variable Specifier
        Rule rule = Grammar.Resource;

        valid(rule, LIDENTIFIER);
        valid(rule, UIDENTIFIER, LBRACE, RBRACE);
        valid(rule, LIDENTIFIER, SPECIFY, CHAR_LITERAL);
    }

    @Test
    public void testReturn() {
        // Return: "return" Expression?
        Rule rule = Grammar.Return;

        valid(rule, RETURN);
        valid(rule, RETURN, CHAR_LITERAL);
    }

    @Test
    public void testSatisfiedTypes() {
        // SatisfiedTypes: "satisfies" Type ("&" Type)*
        Rule rule = Grammar.SatisfiedTypes;

        valid(rule, SATISFIES, UIDENTIFIER);
        valid(rule, SATISFIES, UIDENTIFIER, INTERSECTION_OP, UIDENTIFIER);
        valid(rule, SATISFIES, UIDENTIFIER, SMALLER_OP, UIDENTIFIER, LARGER_OP, INTERSECTION_OP, UIDENTIFIER);
    }

    @Test
    public void testSatisfiesCondition() {
        // SatisfiesCondition: "satisfies" Type Type
        Rule rule = Grammar.SatisfiesCondition;

        valid(rule, SATISFIES, UIDENTIFIER, UIDENTIFIER);
        valid(rule, SATISFIES, UIDENTIFIER, MEMBER_OP, UIDENTIFIER, UIDENTIFIER);
    }

    @Test
    public void testSelfReference() {
        // SelfReference: "this" | "super" | "outer"
        Rule rule = Grammar.SelfReference;

        valid(rule, THIS);
        valid(rule, SUPER);
        valid(rule, OUTER);
    }

    @Test
    @Ignore
    public void testSequence() {
        // Sequence: Expression ("," Expression)* | Expression "..."
        Rule rule = Grammar.Sequence;

        // TODO: Implement
    }

    @Test
    public void testSequencedParam() {
        // SequencedParam: Annotation* UnionType "..." MemberName
        Rule rule = Grammar.SequencedParam;

        valid(rule, UIDENTIFIER, ELLIPSIS, LIDENTIFIER);
        valid(rule, UIDENTIFIER, UNION_OP, UIDENTIFIER, ELLIPSIS, LIDENTIFIER);
    }

    @Test
    public void testSequencedType() {
        // SequencedType: TypeName "..."
        Rule rule = Grammar.SequencedType;

        valid(rule, UIDENTIFIER, ELLIPSIS);
    }

    @Test
    public void testSequencedTypeParam() {
        // SequencedTypeParam: TypeName "..."
        Rule rule = Grammar.SequencedTypeParam;

        valid(rule, UIDENTIFIER, ELLIPSIS);
    }

    @Test
    public void testSequenceInstantiation() {
        // SequenceInstantiation: "{" Sequence? "}" ;
        Rule rule = Grammar.SequenceInstantiation;

        valid(rule, LBRACE, RBRACE);
        valid(rule, LBRACE, LIDENTIFIER, ELLIPSIS, RBRACE);
        valid(rule, LBRACE, CHAR_LITERAL, COMMA, CHAR_LITERAL, RBRACE);
    }

    @Test
    public void testSimpleAttribute() {
        // SimpleAttribute: AttributeHeader ( (Specifier | Initializer)? ";" | NamedArguments )
        Rule rule = Grammar.SimpleAttribute;

        valid(rule, VALUE_MODIFIER, LIDENTIFIER, SEMICOLON);
        valid(rule, VALUE_MODIFIER, LIDENTIFIER, SPECIFY, STRING_LITERAL, SEMICOLON);
        valid(rule, VALUE_MODIFIER, LIDENTIFIER, ASSIGN_OP, STRING_LITERAL, SEMICOLON);
        valid(rule, VALUE_MODIFIER, LIDENTIFIER, LBRACE, RBRACE);
        valid(rule, UIDENTIFIER, LIDENTIFIER, LBRACE, RBRACE);
    }

    @Test
    public void testSimpleParam() {
        // SimpleParam: UnionType MemberName
        Rule rule = Grammar.SimpleParam;

        valid(rule, UIDENTIFIER, LIDENTIFIER);
        valid(rule, UIDENTIFIER, UNION_OP, UIDENTIFIER, LIDENTIFIER);
    }

    @Test
    public void testSpecification() {
        // Specification: MemberName Specifier ";"
        Rule rule = Grammar.Specification;

        valid(rule, LIDENTIFIER, SPECIFY, STRING_LITERAL, SEMICOLON);
    }

    @Test
    public void testSpecifiedNamedArgument() {
        // SpecifiedNamedArgument: MemberName Specifier ";"
        Rule rule = Grammar.SpecifiedNamedArgument;

        valid(rule, LIDENTIFIER, SPECIFY, STRING_LITERAL, SEMICOLON);
    }

    @Test
    public void testSpecifier() {
        // Specifier: "=" Expression
        Rule rule = Grammar.Specifier;

        valid(rule, SPECIFY, THIS);
    }

    @Test
    @Ignore
    public void testStatement() {
        // Statement: ExpressionStatement | Specification | DirectiveStatement | ControlStructure
        Rule rule = Grammar.Statement;

        // TODO: Implement
    }

    @Test
    public void testStringTemplate() {
        // StringTemplate: StringLiteral (Expression StringLiteral)+
        Rule rule = Grammar.StringTemplate;

        valid(rule, STRING_LITERAL, THIS, STRING_LITERAL);
        valid(rule, STRING_LITERAL, THIS, STRING_LITERAL, SUPER, STRING_LITERAL);
    }

    @Test
    public void testSwitch() {
        // Switch: "switch" "(" Expression ")"
        Rule rule = Grammar.Switch;

        valid(rule, SWITCH_CLAUSE, LPAREN, CHAR_LITERAL, RPAREN);
    }

    @Test
    public void testSwitchCaseElse() {
        // SwitchCaseElse: Switch ( Cases | "{" Cases "}" )
        Rule rule = Grammar.SwitchCaseElse;

        valid(rule, SWITCH_CLAUSE, LPAREN, CHAR_LITERAL, RPAREN, CASE_CLAUSE, LPAREN, CHAR_LITERAL, RPAREN, LBRACE, RBRACE);
        valid(rule, SWITCH_CLAUSE, LPAREN, CHAR_LITERAL, RPAREN, CASE_CLAUSE, LPAREN, CHAR_LITERAL, RPAREN, LBRACE, RBRACE, ELSE_CLAUSE,
                LBRACE, RBRACE);
        valid(rule, SWITCH_CLAUSE, LPAREN, CHAR_LITERAL, RPAREN, LBRACE, CASE_CLAUSE, LPAREN, CHAR_LITERAL, RPAREN, LBRACE, RBRACE,
                RBRACE);
    }

    @Test
    public void testThrow() {
        // Throw: "throw" Expression?
        Rule rule = Grammar.Throw;

        valid(rule, THROW);
        valid(rule, THROW, CHAR_LITERAL);
    }

    @Test
    @Ignore
    public void testTimeLiteral() {
        // TimeLiteral: "'" Digit{1,2} ":" Digit{2} ( ":" Digit{2} ( ":" Digit{3} )? )? (" " "AM"|"PM")? (" " Character{3,4})? "'"
        Rule rule = Grammar.TimeLiteral;

        // TODO: Implement
    }

    @Test
    public void testTry() {
        // Try: "try" ("(" Resource ")")? Block
        Rule rule = Grammar.Try;

        valid(rule, TRY_CLAUSE, LBRACE, RBRACE);
        valid(rule, TRY_CLAUSE, LPAREN, LIDENTIFIER, RPAREN, LBRACE, RBRACE);
    }

    @Test
    public void testTryCatchFinally() {
        // TryCatchFinally: Try Catch* Finally?
        Rule rule = Grammar.TryCatchFinally;

        valid(rule, TRY_CLAUSE, LBRACE, RBRACE);
        valid(rule, TRY_CLAUSE, LBRACE, RBRACE, CATCH_CLAUSE, LPAREN, LIDENTIFIER, RPAREN, LBRACE, RBRACE);
        valid(rule, TRY_CLAUSE, LBRACE, RBRACE, CATCH_CLAUSE, LPAREN, LIDENTIFIER, RPAREN, LBRACE, RBRACE, CATCH_CLAUSE, LPAREN,
                LIDENTIFIER, RPAREN, LBRACE, RBRACE);
        valid(rule, TRY_CLAUSE, LBRACE, RBRACE, FINALLY_CLAUSE, LBRACE, RBRACE);
        valid(rule, TRY_CLAUSE, LBRACE, RBRACE, CATCH_CLAUSE, LPAREN, LIDENTIFIER, RPAREN, LBRACE, RBRACE, FINALLY_CLAUSE, LBRACE, RBRACE);
    }

    @Test
    public void testType() {
        // Type: TypeNameWithArguments ("." TypeNameWithArguments)*
        Rule rule = Grammar.Type;

        valid(rule, UIDENTIFIER);
        valid(rule, UIDENTIFIER, SMALLER_OP, UIDENTIFIER, LARGER_OP);
        valid(rule, UIDENTIFIER, MEMBER_OP, UIDENTIFIER, SMALLER_OP, UIDENTIFIER, LARGER_OP);
        valid(rule, UIDENTIFIER, SMALLER_OP, UIDENTIFIER, LARGER_OP, MEMBER_OP, UIDENTIFIER);
    }

    @Test
    public void testTypeAlias() {
        // TypeAlias: TypeName "="
        Rule rule = Grammar.TypeAlias;

        valid(rule, UIDENTIFIER, SPECIFY);
    }

    @Test
    public void testTypeArguments() {
        // TypeArguments: "<" (UnionType ",")* (UnionType | SequencedType) ">"
        Rule rule = Grammar.TypeArguments;

        valid(rule, SMALLER_OP, UIDENTIFIER, LARGER_OP);
        valid(rule, SMALLER_OP, UIDENTIFIER, COMMA, UIDENTIFIER, LARGER_OP);
        valid(rule, SMALLER_OP, UIDENTIFIER, ELLIPSIS, LARGER_OP);
        valid(rule, SMALLER_OP, UIDENTIFIER, UNION_OP, UIDENTIFIER, COMMA, UIDENTIFIER, ELLIPSIS, LARGER_OP);
    }

    @Test
    public void testTypeConstraint() {
        // TypeConstraint: "given" TypeName TypeParams? Params? TypeConstraintInheritance
        Rule rule = Grammar.TypeConstraint;

        valid(rule, TYPE_CONSTRAINT, UIDENTIFIER);
        valid(rule, TYPE_CONSTRAINT, UIDENTIFIER, SMALLER_OP, UIDENTIFIER, LARGER_OP);
        valid(rule, TYPE_CONSTRAINT, UIDENTIFIER, LPAREN, UIDENTIFIER, ELLIPSIS, LIDENTIFIER, RPAREN);
        valid(rule, TYPE_CONSTRAINT, UIDENTIFIER, CASE_TYPES, UIDENTIFIER);
    }

    @Test
    public void testTypeConstraintInheritance() {
        // TypeConstraintInheritance: CaseTypes? Metatypes? SatisfiedTypes? AbstractedType?
        Rule rule = Grammar.TypeConstraintInheritance;

        valid(rule);
        valid(rule, CASE_TYPES, UIDENTIFIER);
        valid(rule, IS_OP, UIDENTIFIER);
        valid(rule, SATISFIES, UIDENTIFIER);
        valid(rule, ABSTRACTED_TYPE, UIDENTIFIER);
        valid(rule, CASE_TYPES, UIDENTIFIER, IS_OP, UIDENTIFIER, SATISFIES, UIDENTIFIER, ABSTRACTED_TYPE, UIDENTIFIER);
    }

    @Test
    public void testTypeConstraints() {
        // TypeConstraints: TypeConstraint+
        Rule rule = Grammar.TypeConstraints;

        valid(rule, TYPE_CONSTRAINT, UIDENTIFIER);
        valid(rule, TYPE_CONSTRAINT, UIDENTIFIER, TYPE_CONSTRAINT, UIDENTIFIER);

        invalid(rule);
    }

    @Test
    public void testTypeDeclaration() {
        // TypeDeclaration: Class | Object | Interface
        Rule rule = Grammar.TypeDeclaration;

        valid(rule, CLASS_DEFINITION, UIDENTIFIER, LPAREN, RPAREN, LBRACE, RBRACE);
        valid(rule, OBJECT_DEFINITION, LIDENTIFIER, LBRACE, RBRACE);
        valid(rule, INTERFACE_DEFINITION, UIDENTIFIER, LBRACE, RBRACE);
    }

    @Test
    public void testTypedVariable() {
        // TypedVariable: UnionType MemberName
        Rule rule = Grammar.TypedVariable;

        valid(rule, UIDENTIFIER, LIDENTIFIER);
        valid(rule, UIDENTIFIER, UNION_OP, UIDENTIFIER, LIDENTIFIER);
    }

    @Test
    public void testTypeMeta() {
        // TypeMeta: Type
        Rule rule = Grammar.TypeMeta;

        valid(rule, UIDENTIFIER);
    }

    @Test
    public void testTypeNameWithArguments() {
        // TypeNameWithArguments: TypeName TypeArguments?
        Rule rule = Grammar.TypeNameWithArguments;

        valid(rule, UIDENTIFIER);
        valid(rule, UIDENTIFIER, SMALLER_OP, UIDENTIFIER, LARGER_OP);
        valid(rule, UIDENTIFIER, SMALLER_OP, UIDENTIFIER, ELLIPSIS, LARGER_OP);
    }

    @Test
    public void testTypeParam() {
        // TypeParam: Variance? TypeName
        Rule rule = Grammar.TypeParam;

        valid(rule, UIDENTIFIER);
        valid(rule, IN_OP, UIDENTIFIER);
        valid(rule, OUT, UIDENTIFIER);
    }

    @Test
    public void testTypeParams() {
        // TypeParams: "<" (TypeParam ",")* (TypeParam | SequencedTypeParam) ">"
        Rule rule = Grammar.TypeParams;

        valid(rule, SMALLER_OP, UIDENTIFIER, LARGER_OP);
        valid(rule, SMALLER_OP, IN_OP, UIDENTIFIER, LARGER_OP);
        valid(rule, SMALLER_OP, UIDENTIFIER, COMMA, UIDENTIFIER, LARGER_OP);
        valid(rule, SMALLER_OP, UIDENTIFIER, ELLIPSIS, LARGER_OP);
        valid(rule, SMALLER_OP, UIDENTIFIER, COMMA, UIDENTIFIER, ELLIPSIS, LARGER_OP);
    }

    @Test
    public void testTypeSpecifier() {
        // TypeSpecifier: "=" Type
        Rule rule = Grammar.TypeSpecifier;

        valid(rule, SPECIFY, UIDENTIFIER);
    }

    @Test
    public void testUnionType() {
        // UnionType: IntersectionType ("|" IntersectionType)*
        Rule rule = Grammar.UnionType;

        valid(rule, UIDENTIFIER);
        valid(rule, UIDENTIFIER, UNION_OP, UIDENTIFIER);
    }

    @Test
    public void testValueMeta() {
        // ValueMeta: MemberName TypeArguments?
        Rule rule = Grammar.ValueMeta;

        valid(rule, LIDENTIFIER);
        valid(rule, LIDENTIFIER, SMALLER_OP, UIDENTIFIER, LARGER_OP);
    }

    @Test
    @Ignore
    public void testValueReference() {
        // ValueReference: (Receiver ".")? MemberName
        Rule rule = Grammar.ValueReference;

        // TODO: Implement
    }

    @Test
    public void testVariable() {
        // Variable: UnionType? MemberName
        Rule rule = Grammar.Variable;

        valid(rule, LIDENTIFIER);
        valid(rule, UIDENTIFIER, LIDENTIFIER);
        valid(rule, UIDENTIFIER, UNION_OP, UIDENTIFIER, LIDENTIFIER);
    }

    @Test
    public void testVariance() {
        // Variance: "out" | "in"
        Rule rule = Grammar.Variance;

        valid(rule, OUT);
        valid(rule, IN_OP);
    }

    @Test
    public void testWhile() {
        // While: LoopCondition Block
        Rule rule = Grammar.While;

        valid(rule, WHILE_CLAUSE, LPAREN, LIDENTIFIER, RPAREN, LBRACE, RBRACE);
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
