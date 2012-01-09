package org.ceylon.idea.lang.parser;

import org.ceylon.idea.lang.parser.rule.ComplexRule;
import org.ceylon.idea.lang.parser.rule.DummyRule;
import org.ceylon.idea.lang.parser.rule.NotImplementedRule;
import org.ceylon.idea.lang.parser.rule.Rule;

import static org.ceylon.idea.lang.lexer.CeylonToken.*;
import static org.ceylon.idea.lang.parser.rule.Rules.*;


@SuppressWarnings("unused")
public class Grammar {

    /**
     * {@code Block:  "{" (Declaration | Statement)* "}"	 }
     *
     * @see #deferredInit()
     */
    public static final ComplexRule Block = rule("Block");

    /**
     * {@code Expression:  Primary | OperatorExpression	 }
     *
     * @see #deferredInit()
     */
    public static final ComplexRule Expression = rule("Expression");

    /**
     * {@code NamedArguments:  "{" NamedArgument* Sequence? "}"	 }
     *
     * @see #deferredInit()
     */
    public static final ComplexRule NamedArguments = rule("NamedArguments");

    /**
     * {@code Annotation:  MemberName ( Arguments | Literal+ )?	 }
     *
     * @see #deferredInit()
     */
    public static final ComplexRule Annotation = rule("Annotation");

    /**
     * {@code Param:  Annotation* (SimpleParam | CallableParam | EntryParamPair) }
     *
     * @see #deferredInit()
     */
    public static final ComplexRule Param = rule("Param");

    /**
     * {@code AnnotationName:  LIdentifier }
     */
    public static final Rule AnnotationName = rule("AnnotationName").one(LIDENTIFIER);

    /**
     * {@code TypeName:  UIdentifier }
     */
    public static final Rule TypeName = rule("TypeName").one(UIDENTIFIER);

    /**
     * {@code MemberName:  LIdentifier	 }
     */
    public static final Rule MemberName = rule("MemberName").one(LIDENTIFIER);

    /**
     * {@code TypeArguments:  "<" (UnionType ",")* (UnionType | SequencedType) ">" }
     */
    public static final Rule TypeArguments = new DummyRule("TypeArguments");

    /**
     * {@code TypeNameWithArguments:  TypeName TypeArguments?	 }
     */
    public static final Rule TypeNameWithArguments = rule("TypeNameWithArguments").one(TypeName).zeroOrOne(TypeArguments);

    /**
     * {@code Type:  TypeNameWithArguments ("." TypeNameWithArguments)*	 }
     */
    public static final Rule Type = rule("Type").one(TypeNameWithArguments).zeroOrMore(MEMBER_OP, TypeNameWithArguments);

    /**
     * {@code Abbreviation:  "?" | "[]"	 }
     */
    public static final Rule Abbreviation = any(QMARK, ARRAY);

    /**
     * {@code AbbreviatedType:  Type Abbreviation* }
     */
    public static final Rule AbbreviatedType = rule("AbbreviatedType").one(Type).zeroOrMore(Abbreviation);

    /**
     * {@code EntryType:  AbbreviatedType ("->" AbbreviatedType)? }
     */
    public static final Rule EntryType = rule("EntryType").one(AbbreviatedType).zeroOrOne(ENTRY_OP, AbbreviatedType);

    /**
     * {@code IntersectionType:  EntryType ("&" EntryType)*	 }
     */
    public static final Rule IntersectionType = rule("IntersectionType").one(EntryType).zeroOrMore(INTERSECTION_OP, EntryType);

    /**
     * {@code UnionType:  IntersectionType ("|" IntersectionType)* }
     */
    public static final Rule UnionType = rule("UnionType").one(IntersectionType).zeroOrMore(UNION_OP, IntersectionType);

    /**
     * {@code CompilerAnnotation : "@" AnnotationName ("[" StringLiteral "]")? }
     */
    public static final Rule CompilerAnnotation = rule("CompilerAnnotation").sequence(COMPILER_ANNOTATION, AnnotationName).zeroOrOne(INDEX_OP, STRING_LITERAL, RBRACKET);

    /**
     * {@code CompilerAnnotations: CompilerAnnotation* }
     */
    public static final Rule CompilerAnnotations = zeroOrMore(CompilerAnnotation);

    /**
     * {@code PackageName:  PIdentifier }
     */
    public static final Rule PackageName = rule("PackageName").one(LIDENTIFIER);

    /**
     * {@code FullPackageName:  PackageName ("." PackageName)* }
     */
    public static final Rule FullPackageName = rule("FullPackageName").one(PackageName).zeroOrMore(MEMBER_OP, PackageName);

    /**
     * {@code ImportWildcard:  "..." }
     */
    public static final Rule ImportWildcard = rule("ImportWildcard").one(ELLIPSIS);

    /**
     * {@code TypeAlias:  TypeName "=" }
     */
    public static final Rule TypeAlias = rule("TypeAlias").sequence(TypeName, SPECIFY);

    /**
     * {@code ImportTypeElement:  TypeAlias? TypeName }
     */
    public static final Rule ImportTypeElement = rule("ImportTypeElement").zeroOrOne(TypeAlias).one(TypeName);

    /**
     * {@code MethodAttributeAlias:  MemberName "="	 }
     */
    public static final Rule MethodAttributeAlias = rule("MethodAttributeAlias").sequence(MemberName, SPECIFY);

    /**
     * {@code ImportMethodAttributeElement:  MethodAttributeAlias? MemberName }
     */
    public static final Rule ImportMethodAttributeElement = rule("ImportMethodAttributeElement").zeroOrOne(MethodAttributeAlias).one(MemberName);

    /**
     * {@code ImportElement:  ImportTypeElement | ImportMethodAttributeElement }
     */
    public static final Rule ImportElement = any(ImportTypeElement, ImportMethodAttributeElement);

    /**
     * {@code ImportElements:  ImportElement ("," ImportElement)* ("," ImportWildcard)? | ImportWildcard }
     */
    public static final Rule ImportElements = rule("ImportElements")
            .any(
                    sequence(ImportElement, zeroOrMore(COMMA, ImportElement), zeroOrOne(COMMA, ImportWildcard)),
                    ImportWildcard);

    /**
     * {@code Import:  "import" FullPackageName "{" ImportElements? "}" }
     */
    public static final Rule Import = rule("Import").sequence(IMPORT, FullPackageName).one(LBRACE).zeroOrOne(ImportElements).one(RBRACE);

    /**
     * {@code Sequence:  Expression ("," Expression)* | Expression "..."	 }
     */
    public static final Rule Sequence = new DummyRule("Sequence");

    /**
     * {@code PositionalArguments:  "(" Expression ("," Expression)* ("," Sequence)? | Sequence? ")" }
     */
    public static final Rule PositionalArguments = rule("PositionalArguments")
            .one(LPAREN)
            .any(
                    sequence(Expression, zeroOrMore(COMMA, Expression), zeroOrOne(COMMA, Sequence)),
                    zeroOrOne(Sequence))
            .one(RPAREN);

    /**
     * {@code OperatorExpression:  ????? } // TODO: Check out language spec
     */
    public static final Rule OperatorExpression = new DummyRule("OperatorExpression");

    /**
     * {@code IntegerLiteral:  Digits Magnitude?	 }
     */
    public static final Rule IntegerLiteral = new DummyRule("IntegerLiteral");

    /**
     * {@code FloatLiteral:  Digits ("." FractionalDigits (Exponent | Magnitude | FractionalMagnitude)? | FractionalMagnitude)	 }
     */
    public static final Rule FloatLiteral = new DummyRule("FloatLiteral");

    /**
     * {@code CharacterLiteral:  "`" Character "`"	 }
     */
    public static final Rule CharacterLiteral = new DummyRule("CharacterLiteral");

    /**
     * {@code QuotedLiteral:  "'" QuotedLiteralCharacter* "'"	 }
     */
    public static final Rule QuotedLiteral = new DummyRule("QuotedLiteral");

    /**
     * {@code Literal:  IntegerLiteral | FloatLiteral | CharacterLiteral | StringLiteral | QuotedLiteral	 }
     */
    public static final Rule Literal = any(IntegerLiteral, FloatLiteral, CharacterLiteral, STRING_LITERAL, QuotedLiteral);

    /**
     * {@code StringTemplate:  StringLiteral (Expression StringLiteral)+	 }
     */
    public static final Rule StringTemplate = new DummyRule("StringTemplate");

    /**
     * {@code SelfReference:  "this" | "super" | "outer"	 }
     */
    public static final Rule SelfReference = any(THIS, SUPER, OUTER);

    /**
     * {@code ParExpression:  "(" Expression ")"	 }
     */
    public static final Rule ParExpression = sequence(LPAREN, Expression, RPAREN);

    /**
     * {@code Atom:  Literal | StringTemplate | SelfReference | ParExpression	 }
     */
    public static final Rule Atom = any(Literal, StringTemplate, SelfReference, ParExpression);

    /**
     * {@code Meta:  TypeMeta | MethodMeta | AttributeMeta | FunctionMeta | ValueMeta	 }
     */
    public static final Rule Meta = new DummyRule("Meta");

    /**
     * {@code MemberReference:  CallableReference | ValueReference	 }
     */
    public static final Rule MemberReference = new DummyRule("MemberReference");

    /**
     * {@code Invocation:  Primary Arguments | SequenceInstantiation	 }
     */
    public static final Rule Invocation = new DummyRule("Invocation");

    /**
     * {@code Primary:  Atom | Meta | MemberReference | Invocation	 }
     */
    public static final Rule Primary = any(Atom, Meta, MemberReference, Invocation);

    /**
     * {@code Specifier:  "=" Expression	 }
     */
    public static final Rule Specifier = sequence(SPECIFY, Expression);

    /**
     * {@code SpecifiedNamedArgument:  MemberName Specifier ";"	 }
     */
    public static final Rule SpecifiedNamedArgument = rule("SpecifiedNamedArgument").sequence(MemberName, Specifier, SEMICOLON);

    /**
     * {@code LocalNamedArgument:  (UnionType | "value") MemberName (Block | NamedArguments)	 }
     */
    public static final Rule LocalNamedArgument = rule("LocalNamedArgument").any(UnionType, VALUE_MODIFIER).one(MemberName).any(Block, NamedArguments);

    /**
     * {@code SimpleParam:  UnionType MemberName	 }
     */
    public static final Rule SimpleParam = rule("SimpleParam").sequence(UnionType, MemberName);

    /**
     * {@code EntryParamPair:  SimpleParam "->" SimpleParam	 }
     */
    public static final Rule EntryParamPair = rule("EntryParamPair").sequence(SimpleParam, ENTRY_OP, SimpleParam);

    /**
     * {@code DefaultParam:  Param Specifier	 }
     */
    public static final Rule DefaultParam = new NotImplementedRule("DefaultParam");

    /**
     * {@code SequencedParam:  Annotation* UnionType "..." MemberName	 }
     */
    public static final Rule SequencedParam = rule("SequencedParam").zeroOrMore(Annotation).sequence(UnionType, ELLIPSIS, MemberName);

    /**
     * {@code Params:   "(" Param ("," Param)* ("," DefaultParam)* ("," SequencedParam)? |  DefaultParam ("," DefaultParam)* ("," SequencedParam)? |  SequencedParam? ")"	 }
     */
    public static final Rule Params = rule("Params")
            .one(LPAREN)
            .any(
                    sequence(Param, zeroOrMore(COMMA, Param), zeroOrMore(COMMA, DefaultParam), zeroOrOne(COMMA, SequencedParam)),
                    sequence(DefaultParam, zeroOrMore(COMMA, DefaultParam), zeroOrOne(COMMA, SequencedParam)),
                    zeroOrOne(SequencedParam))
            .one(RPAREN);

    /**
     * {@code CallableParam:  (UnionType | "void") MemberName Params+	 }
     */
    public static final Rule CallableParam = rule("CallableParam").any(UnionType, VOID_MODIFIER).one(MemberName).oneOrMore(Params);

    /**
     * {@code FunctionalNamedArgument:  (UnionType | "function" | "void") MemberName Params+ (Block | NamedArguments)	 }
     */
    public static final Rule FunctionalNamedArgument = rule("FunctionalNamedArgument").any(UnionType, FUNCTION_MODIFIER, VOID_MODIFIER).one(MemberName).oneOrMore(Params).any(Block, NamedArguments);

    /**
     * {@code FunctionalBody:  Params? ( Block | "(" Expression ")" )	 }
     */
    public static final Rule FunctionalBody = rule("FunctionalBody").zeroOrOne(Params).any(Block, sequence(LPAREN, Expression, RPAREN));

    /**
     * {@code FunctionalArguments:  (MemberName FunctionalBody)+	 }
     */
    public static final Rule FunctionalArguments = rule("FunctionalArguments").sequence(MemberName, FunctionalBody);

    /**
     * {@code Object:  Annotation* ObjectHeader ClassBody	 }
     */
    public static final Rule Object = new DummyRule("Object");

    /**
     * {@code NamedArgument:  SpecifiedNamedArgument | LocalNamedArgument | FunctionalNamedArgument | Object	 }
     */
    public static final Rule NamedArgument = any(SpecifiedNamedArgument, LocalNamedArgument, FunctionalNamedArgument, Object);

    /**
     * {@code Arguments:  PositionalArguments FunctionalArguments? | NamedArguments }
     */
    public static final Rule Arguments = rule("Arguments").any(sequence(PositionalArguments, zeroOrOne(FunctionalArguments)), NamedArguments);

    /**
     * {@code Method:  Annotation* MethodHeader (Block | NamedArguments | Specifier? ";")	 }
     */
    public static final Rule Method = new DummyRule("Method");

    /**
     * {@code AttributeHeader:  (UnionType | "value") MemberName	 }
     */
    public static final Rule AttributeHeader = rule("AttributeHeader").any(UnionType, VALUE_MODIFIER).one(MemberName);

    /**
     * {@code SimpleAttribute:  AttributeHeader ( (Specifier | Initializer)? ";" | NamedArguments )	 }
     */
    public static final Rule SimpleAttribute = new DummyRule("SimpleAttribute");

    /**
     * {@code AttributeGetter:  AttributeHeader Block	 }
     */
    public static final Rule AttributeGetter = rule("AttributeGetter").sequence(AttributeHeader, Block);

    /**
     * {@code AttributeSetter:  "assign" MemberName Block }
     */
    public static final Rule AttributeSetter = rule("AttributeSetter").sequence(ASSIGN, MemberName, Block);

    /**
     * {@code Attribute:  Annotation* (SimpleAttribute | AttributeGetter | AttributeSetter)	 }
     */
    public static final Rule Attribute = rule("Attribute").zeroOrMore(Annotation).any(SimpleAttribute, AttributeGetter, AttributeSetter);

    /**
     * {@code TypeDeclaration:  Class | Object | Interface	 }
     */
    public static final Rule TypeDeclaration = new DummyRule("TypeDeclaration");

    /**
     * {@code Declaration:  Method | Attribute | TypeDeclaration }
     */
    public static final Rule Declaration = any(Method, Attribute, TypeDeclaration);

    /**
     * {@code CompilationUnit : (CompilerAnnotation+ ";")? import* (CompilerAnnotations Declaration)* EOF }
     */
    public static final Rule CompilationUnit = rule("CompilationUnit").zeroOrOne(CompilerAnnotation, CompilerAnnotations, SEMICOLON).zeroOrMore(Import).zeroOrMore(CompilerAnnotations, Declaration);

    /**
     * {@code AbstractedType:  "abstracts" Type	 }
     */
    public static final Rule AbstractedType = new NotImplementedRule("AbstractedType");

    /**
     * {@code AdaptedTypes:  "adapts" Type ("&" Type)*	 }
     */
    public static final Rule AdaptedTypes = new NotImplementedRule("AdaptedTypes");

    /**
     * {@code Assignment:  ":=" | ".=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^="| "~=" | "&&=" | "||=" ;	 }
     */
    public static final Rule Assignment = new NotImplementedRule("Assignment");

    /**
     * {@code AttributeMeta:  Type "." MemberName	 }
     */
    public static final Rule AttributeMeta = new NotImplementedRule("AttributeMeta");

    /**
     * {@code BooleanCondition:  Expression }
     */
    public static final Rule BooleanCondition = new NotImplementedRule("BooleanCondition");

    /**
     * {@code Break:  "break" }
     */
    public static final Rule Break = new NotImplementedRule("Break");

    /**
     * {@code CallableReference:  MethodReference | InitializerReference	 }
     */
    public static final Rule CallableReference = new NotImplementedRule("CallableReference");

    /**
     * {@code CallableVariable:  (UnionType | "void")? MemberName Params+	 }
     */
    public static final Rule CallableVariable = new NotImplementedRule("CallableVariable");

    /**
     * {@code Case:  Expression ("," Expression)* | "is" UnionType | "satisfies" Type	 }
     */
    public static final Rule Case = new NotImplementedRule("Case");

    /**
     * {@code CaseItem:  "case" "(" Case ")" Block	 }
     */
    public static final Rule CaseItem = new NotImplementedRule("CaseItem");

    /**
     * {@code Cases:  CaseItem+ DefaultCaseItem?	 }
     */
    public static final Rule Cases = new NotImplementedRule("Cases");

    /**
     * {@code CaseType:  MemberName | Type	 }
     */
    public static final Rule CaseType = new NotImplementedRule("CaseType");

    /**
     * {@code CaseTypes:  "of" CaseType ("|" CaseType)*	 }
     */
    public static final Rule CaseTypes = new NotImplementedRule("CaseTypes");

    /**
     * {@code Catch:  "catch" "(" Variable ")" Block	 }
     */
    public static final Rule Catch = new NotImplementedRule("Catch");

    /**
     * {@code Character:  ~("`" | "\" | Tab | "Formfeed | Newline | Return | Backspace) | EscapeSequence	 }
     */
    public static final Rule Character = new NotImplementedRule("Character");

    /**
     * {@code Class:  Annotation* ClassHeader (ClassBody | TypeSpecifier ";")	 }
     */
    public static final Rule Class = new NotImplementedRule("Class");

    /**
     * {@code ClassBody:  "{" (Declaration | Statement)* "}"	 }
     */
    public static final Rule ClassBody = new NotImplementedRule("ClassBody");

    /**
     * {@code ClassHeader:  "class" TypeName TypeParams? Params ClassInheritance TypeConstraints?	 }
     */
    public static final Rule ClassHeader = new NotImplementedRule("ClassHeader");

    /**
     * {@code ClassInheritance:  CaseTypes? "Metatypes? ExtendedType? SatisfiedTypes?	 }
     */
    public static final Rule ClassInheritance = new NotImplementedRule("ClassInheritance");

    /**
     * {@code ConcreteType:  "this" "is"	 }
     */
    public static final Rule ConcreteType = new NotImplementedRule("ConcreteType");

    /**
     * {@code ConditionalTypes:  SatisfiedTypes Conditions	 }
     */
    public static final Rule ConditionalTypes = new NotImplementedRule("ConditionalTypes");

    /**
     * {@code Condition:  BooleanCondition | IsCondition | ExistsOrNonemptyCondition | SatisfiesCondition	 }
     */
    public static final Rule Condition = new NotImplementedRule("Condition");

    /**
     * {@code Conditions:  "if" "(" Condition ("&&" Condition)* ")"	 }
     */
    public static final Rule Conditions = new NotImplementedRule("Conditions");

    /**
     * {@code Continue:  "continue"	 }
     */
    public static final Rule Continue = new NotImplementedRule("Continue");

    /**
     * {@code ControlStructure:  IfElse | SwitchCaseElse | While | ForFail | TryCatchFinally	 }
     */
    public static final Rule ControlStructure = new NotImplementedRule("ControlStructure");

    /**
     * {@code DateLiteral:   "'"  Digit{1,2} "/" Digit{1,2} "/" Digit{4}  "'"	 }
     */
    public static final Rule DateLiteral = new NotImplementedRule("DateLiteral");

    /**
     * {@code DefaultCaseItem:  "else" Block	 }
     */
    public static final Rule DefaultCaseItem = new NotImplementedRule("DefaultCaseItem");

    /**
     * {@code Digit:  "0".."9"	 }
     */
    public static final Rule Digit = new NotImplementedRule("Digit");

    /**
     * {@code Digits:  Digit+ | Digit{1..3} ("_" Digit{3})+	 }
     */
    public static final Rule Digits = new NotImplementedRule("Digits");

    /**
     * {@code DimensionAtom:  DimensionConstant | DimensionVariable | "ParenDimension	 }
     */
    public static final Rule DimensionAtom = new NotImplementedRule("DimensionAtom");

    /**
     * {@code DimensionConstant:  "#" IntegerLiteral	 }
     */
    public static final Rule DimensionConstant = new NotImplementedRule("DimensionConstant");

    /**
     * {@code Dimension:  DimensionTerm ("+" DimensionTerm)*	 }
     */
    public static final Rule Dimension = new NotImplementedRule("Dimension");

    /**
     * {@code DimensionTerm:  (DimensionConstant "*")* DimensionAtom	 }
     */
    public static final Rule DimensionTerm = new NotImplementedRule("DimensionTerm");

    /**
     * {@code DimensionVariable:  TypeName | "#" MemberName	 }
     */
    public static final Rule DimensionVariable = new NotImplementedRule("DimensionVariable");

    /**
     * {@code Directive:  Return | Throw | Break | Continue	 }
     */
    public static final Rule Directive = new NotImplementedRule("Directive");

    /**
     * {@code DirectiveStatement:  Directive ";"	 }
     */
    public static final Rule DirectiveStatement = new NotImplementedRule("DirectiveStatement");

    /**
     * {@code Else:  "else" (Block | IfElse)	 }
     */
    public static final Rule Else = new NotImplementedRule("Else");

    /**
     * {@code EntryVariablePair:  Variable "->" Variable	 }
     */
    public static final Rule EntryVariablePair = new NotImplementedRule("EntryVariablePair");

    /**
     * {@code EscapeSequence:  "\" ("b" | "t" | "n" | "f" | "r" | "\" | "\"" | "'" | "`" )	 }
     */
    public static final Rule EscapeSequence = new NotImplementedRule("EscapeSequence");

    /**
     * {@code ExistsOrNonemptyCondition:  ("exists" | "nonempty") (Variable Specifier | MemberName)	 }
     */
    public static final Rule ExistsOrNonemptyCondition = new NotImplementedRule("ExistsOrNonemptyCondition");

    /**
     * {@code Exponent:  ("E"|"e") ("+"|"-")? Digits	 }
     */
    public static final Rule Exponent = new NotImplementedRule("Exponent");

    /**
     * {@code ExpressionStatement:  ( Assignment | IncrementOrDecrement | Invocation ) ";"	 }
     */
    public static final Rule ExpressionStatement = new NotImplementedRule("ExpressionStatement");

    /**
     * {@code ExtendedType:  "extends" ("super" ".")? Type PositionalArguments	 }
     */
    public static final Rule ExtendedType = new NotImplementedRule("ExtendedType");

    /**
     * {@code Fail:  "else" Block	 }
     */
    public static final Rule Fail = new NotImplementedRule("Fail");

    /**
     * {@code Finally:  "finally" Block	 }
     */
    public static final Rule Finally = new NotImplementedRule("Finally");

    /**
     * {@code ForFail:  For Fail?	 }
     */
    public static final Rule ForFail = new NotImplementedRule("ForFail");

    /**
     * {@code For:  "for" "(" ForIterator ")" Block	 }
     */
    public static final Rule For = new NotImplementedRule("For");

    /**
     * {@code ForIterator:  IteratorVariable "in" Expression	 }
     */
    public static final Rule ForIterator = new NotImplementedRule("ForIterator");

    /**
     * {@code FractionalDigits:  Digit+ | (Digit{3} "_")+ Digit{1..3}	 }
     */
    public static final Rule FractionalDigits = new NotImplementedRule("FractionalDigits");

    /**
     * {@code FractionalMagnitude:  "m" | "u" | "n" | "p" | "f"	 }
     */
    public static final Rule FractionalMagnitude = new NotImplementedRule("FractionalMagnitude");

    /**
     * {@code FunctionMeta:  MemberName TypeArguments?	 }
     */
    public static final Rule FunctionMeta = new NotImplementedRule("FunctionMeta");

    /**
     * {@code IdentifierChar:  LowercaseChar | UppercaseChar | Digit	 }
     */
    public static final Rule IdentifierChar = new NotImplementedRule("IdentifierChar");

    /**
     * {@code IfElse:  If Else?	 }
     */
    public static final Rule IfElse = new NotImplementedRule("IfElse");

    /**
     * {@code If:  "if" "(" Condition ")" Block	 }
     */
    public static final Rule If = new NotImplementedRule("If");

    /**
     * {@code IncrementOrDecrement:  "--" | "++" ; }
     */
    public static final Rule IncrementOrDecrement = new NotImplementedRule("IncrementOrDecrement");

    /**
     * {@code Initializer:  ":=" Expression	 }
     */
    public static final Rule Initializer = new NotImplementedRule("Initializer");

    /**
     * {@code InitializerReference:  (Receiver ".")? TypeName TypeArguments?	 }
     */
    public static final Rule InitializerReference = new NotImplementedRule("InitializerReference");

    /**
     * {@code Interface:  Annotation* InterfaceHeader (InterfaceBody | TypeSpecifier ";")	 }
     */
    public static final Rule Interface = new NotImplementedRule("Interface");

    /**
     * {@code InterfaceBody:  "{" Declaration* "}"	 }
     */
    public static final Rule InterfaceBody = new NotImplementedRule("InterfaceBody");

    /**
     * {@code InterfaceHeader:  "interface" TypeName TypeParams? InterfaceInheritance TypeConstraints?	 }
     */
    public static final Rule InterfaceHeader = new NotImplementedRule("InterfaceHeader");

    /**
     * {@code InterfaceInheritance:  CaseTypes? "Metatypes? AdaptedTypes? SatisfiedTypes?	 }
     */
    public static final Rule InterfaceInheritance = new NotImplementedRule("InterfaceInheritance");

    /**
     * {@code Introduction:  "adapt" Type SatisfiedTypes TypeConstraints? ";"	 }
     */
    public static final Rule Introduction = new NotImplementedRule("Introduction");

    /**
     * {@code IsCondition:  "is" (TypedVariable Specifier | UnionType MemberName)	 }
     */
    public static final Rule IsCondition = new NotImplementedRule("IsCondition");

    /**
     * {@code IteratorVariable:  Variable | CallableVariable | EntryVariablePair	 }
     */
    public static final Rule IteratorVariable = new NotImplementedRule("IteratorVariable");

    /**
     * {@code LineComment:  ("//"|"#!") ~(Newline|Return)* (Return Newline | Return | Newline)?	 }
     */
    public static final Rule LineComment = new NotImplementedRule("LineComment");

    /**
     * {@code LoopCondition:  "while" "(" Condition ")"	 }
     */
    public static final Rule LoopCondition = new NotImplementedRule("LoopCondition");

    /**
     * {@code LowercaseChar:  "a".."z" | "_" ;	 }
     */
    public static final Rule LowercaseChar = new NotImplementedRule("LowercaseChar");

    /**
     * {@code Magnitude:  "k" | "M" | "G" | "T" | "P"	 }
     */
    public static final Rule Magnitude = new NotImplementedRule("Magnitude");

    /**
     * {@code "Metatypes:  "is" Type ("&" Type)*	 }
     */
    public static final Rule Metatypes = new NotImplementedRule("Metatypes");

    /**
     * {@code MethodHeader:  (UnionType | "function" | "void") MemberName TypeParams? Params+ Metatypes? TypeConstraints?	 }
     */
    public static final Rule MethodHeader = new NotImplementedRule("MethodHeader");

    /**
     * {@code MethodMeta:  Type "." MemberName TypeArguments?	 }
     */
    public static final Rule MethodMeta = new NotImplementedRule("MethodMeta");

    /**
     * {@code MethodReference:  (Receiver ".")? MemberName TypeArguments?	 }
     */
    public static final Rule MethodReference = new NotImplementedRule("MethodReference");

    /**
     * {@code "MultilineComment:  "/" "*" ( MultilineCommentCharacter | MultilineComment )* "*" "/"	 }
     */
    public static final Rule MultilineComment = new NotImplementedRule("MultilineComment");

    /**
     * {@code MultilineCommentCharacter:  ~("/"|"*") | ("/" ~"*") => "/" | ("*" ~"/") => "*"	 }
     */
    public static final Rule MultilineCommentCharacter = new NotImplementedRule("MultilineCommentCharacter");

    /**
     * {@code ObjectHeader:  "object" MemberName ObjectInheritance	 }
     */
    public static final Rule ObjectHeader = new NotImplementedRule("ObjectHeader");

    /**
     * {@code ObjectInheritance:  ExtendedType? SatisfiedTypes?	 }
     */
    public static final Rule ObjectInheritance = new NotImplementedRule("ObjectInheritance");

    /**
     * {@code OuterReference:  (Receiver ".")? "outer"	 }
     */
    public static final Rule OuterReference = new NotImplementedRule("OuterReference");

    /**
     * {@code "ParenDimension:  "(" Dimension ")"	 }
     */
    public static final Rule ParenDimension = new NotImplementedRule("ParenDimension");

    /**
     * {@code QuotedLiteralCharacter:  ~("'")	 }
     */
    public static final Rule QuotedLiteralCharacter = new NotImplementedRule("QuotedLiteralCharacter");

    /**
     * {@code Receiver:  Primary	 }
     */
    public static final Rule Receiver = new NotImplementedRule("Receiver");

    /**
     * {@code Resource:  MemberName | InitializerReference Arguments | Variable Specifier	 }
     */
    public static final Rule Resource = new NotImplementedRule("Resource");

    /**
     * {@code Retry:  "retry"	 }
     */
    public static final Rule Retry = new NotImplementedRule("Retry");

    /**
     * {@code Return:  "return" Expression?	 }
     */
    public static final Rule Return = new NotImplementedRule("Return");

    /**
     * {@code SatisfiedTypes:  "satisfies" Type ("&" Type)*	 }
     */
    public static final Rule SatisfiedTypes = new NotImplementedRule("SatisfiedTypes");

    /**
     * {@code SatisfiesCondition:  "satisfies" Type Type	 }
     */
    public static final Rule SatisfiesCondition = new NotImplementedRule("SatisfiesCondition");

    /**
     * {@code SequencedTypeParam:  TypeName "..."	 }
     */
    public static final Rule SequencedTypeParam = new NotImplementedRule("SequencedTypeParam");

    /**
     * {@code SequencedType:  TypeName "..."	 }
     */
    public static final Rule SequencedType = new NotImplementedRule("SequencedType");

    /**
     * {@code SequenceInstantiation:  "{" Sequence? "}" ;	 }
     */
    public static final Rule SequenceInstantiation = new NotImplementedRule("SequenceInstantiation");

    /**
     * {@code Specification:  MemberName Specifier ";"	 }
     */
    public static final Rule Specification = rule("Specification").sequence(MemberName, Specifier, SEMICOLON);

    /**
     * {@code Statement:  ExpressionStatement | Specification | DirectiveStatement | ControlStructure	 }
     */
    public static final Rule Statement = new DummyRule("Statement");

    /**
     * {@code StringCharacter:  ~( "\" | "\"" ) | EscapeSequence	 }
     */
    public static final Rule StringCharacter = new NotImplementedRule("StringCharacter");

    /**
     * {@code Subtype:  "subtype" | MemberName "." "subtype"	 }
     */
    public static final Rule Subtype = new NotImplementedRule("Subtype");

    /**
     * {@code SwitchCaseElse:  Switch ( Cases | "{" Cases "}" )	 }
     */
    public static final Rule SwitchCaseElse = new NotImplementedRule("SwitchCaseElse");

    /**
     * {@code Switch:  "switch" "(" Expression ")"	 }
     */
    public static final Rule Switch = new NotImplementedRule("Switch");

    /**
     * {@code Throw:  "throw" Expression?	 }
     */
    public static final Rule Throw = new NotImplementedRule("Throw");

    /**
     * {@code TimeLiteral:   "'"  Digit{1,2} ":" Digit{2} ( ":" Digit{2} ( ":" Digit{3} )? )?  (" " "AM"|"PM")?  (" " Character{3,4})?  "'"	 }
     */
    public static final Rule TimeLiteral = new NotImplementedRule("TimeLiteral");

    /**
     * {@code TryCatchFinally:  Try Catch* Finally?	 }
     */
    public static final Rule TryCatchFinally = new NotImplementedRule("TryCatchFinally");

    /**
     * {@code Try:  "try" ("(" Resource ")")? Block	 }
     */
    public static final Rule Try = new NotImplementedRule("Try");

    /**
     * {@code TypeArgument:  UnionType | Dimension	 }
     */
    public static final Rule TypeArgument = new NotImplementedRule("TypeArgument");

    /**
     * {@code TypeConstraint:  "given" TypeName TypeParams? Params? TypeConstraintInheritance	 }
     */
    public static final Rule TypeConstraint = new NotImplementedRule("TypeConstraint");

    /**
     * {@code TypeConstraintInheritance:  CaseTypes? Metatypes? SatisfiedTypes? AbstractedType?	 }
     */
    public static final Rule TypeConstraintInheritance = new NotImplementedRule("TypeConstraintInheritance");

    /**
     * {@code TypeConstraints:  TypeConstraint+	 }
     */
    public static final Rule TypeConstraints = new NotImplementedRule("TypeConstraints");

    /**
     * {@code TypedQuotedLiteral:  TypeName QuotedLiteral	 }
     */
    public static final Rule TypedQuotedLiteral = new NotImplementedRule("TypedQuotedLiteral");

    /**
     * {@code TypedVariable:  UnionType MemberName	 }
     */
    public static final Rule TypedVariable = new NotImplementedRule("TypedVariable");

    /**
     * {@code TypeMeta:  Type	 }
     */
    public static final Rule TypeMeta = new NotImplementedRule("TypeMeta");

    /**
     * {@code TypeParams:  "<" (TypeParam ",")* (TypeParam | SequencedTypeParam) ">"	 }
     */
    public static final Rule TypeParams = new NotImplementedRule("TypeParams");

    /**
     * {@code TypeParam:  Variance? TypeName	 }
     */
    public static final Rule TypeParam = new NotImplementedRule("TypeParam");

    /**
     * {@code TypeSpecifier:  "=" Type	 }
     */
    public static final Rule TypeSpecifier = new NotImplementedRule("TypeSpecifier");

    /**
     * {@code UppercaseChar:  "A".."Z" ;	 }
     */
    public static final Rule UppercaseChar = new NotImplementedRule("UppercaseChar");

    /**
     * {@code ValueMeta:  MemberName TypeArguments?	 }
     */
    public static final Rule ValueMeta = new NotImplementedRule("ValueMeta");

    /**
     * {@code ValueReference:  (Receiver ".")? MemberName	 }
     */
    public static final Rule ValueReference = new NotImplementedRule("ValueReference");

    /**
     * {@code Variable:  UnionType? MemberName	 }
     */
    public static final Rule Variable = new NotImplementedRule("Variable");

    /**
     * {@code Variance:  "out" | "in"	 }
     */
    public static final Rule Variance = new NotImplementedRule("Variance");

    /**
     * {@code While:  LoopCondition Block	 }
     */
    public static final Rule While = new NotImplementedRule("While");

    static {
        deferredInit();
    }

    private static void deferredInit() {
        Block.one(LBRACE).zeroOrAny(Declaration, Statement).one(RBRACE);
        Expression.any(Primary, OperatorExpression);
        NamedArguments.one(LBRACE).zeroOrMore(NamedArgument).zeroOrOne(Sequence).one(RBRACE);
        Param.zeroOrMore(Annotation).any(SimpleParam, CallableParam, EntryParamPair);
        Annotation.one(MemberName).zeroOrAny(Arguments, oneOrMore(Literal));
    }

}
