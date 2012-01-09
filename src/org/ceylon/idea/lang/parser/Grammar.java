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
     */
    public static final ComplexRule Block = rule("Block");

    /**
     * {@code Expression:  Primary | OperatorExpression	 }
     */
    public static final ComplexRule Expression = rule("Expression");

    /**
     * {@code NamedArguments:  "{" NamedArgument* Sequence? "}"	 }
     */
    public static final ComplexRule NamedArguments = rule("NamedArguments");


    /**
     * {@code TypeName:  UIdentifier }
     */
    public static final Rule TypeName = rule("TypeName").one(UIDENTIFIER);

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
     * {@code AnnotationName:  LIdentifier }
     */
    public static final Rule AnnotationName = rule("AnnotationName").one(LIDENTIFIER);

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
     * {@code MemberName:  LIdentifier	 }
     */
    public static final Rule MemberName = rule("MemberName").one(LIDENTIFIER);

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
     * {@code PositionalArguments:  "(" Expression ("," Expression)* ("," Sequence)? | Sequence? ")" }
     */
    public static final Rule PositionalArguments = new DummyRule("PositionalArguments");

    /**
     * {@code OperatorExpression:  ????? } // TODO: Check out language spec
     */
    public static final Rule OperatorExpression = new DummyRule("OperatorExpression");

    /**
     * {@code FunctionalArguments:  (MemberName FunctionalBody)+	 }
     */
    public static final Rule FunctionalArguments = new DummyRule("FunctionalArguments");

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
    public static final Rule SimpleParam = new NotImplementedRule();

    /**
     * {@code CallableParam:  (UnionType | "void") MemberName Params+	 }
     */
    public static final Rule CallableParam = new NotImplementedRule();

    /**
     * {@code EntryParamPair:  SimpleParam "->" SimpleParam	 }
     */
    public static final Rule EntryParamPair = new NotImplementedRule();

    /**
     * {@code DefaultParam:  Param Specifier	 }
     */
    public static final Rule DefaultParam = new NotImplementedRule();

    /**
     * {@code SequencedParam:  Annotation* UnionType "..." MemberName	 }
     */
    public static final Rule SequencedParam = new NotImplementedRule();

    /**
     * {@code Param:  Annotation* (SimpleParam | CallableParam | EntryParamPair) }
     */
    public static final ComplexRule Param = rule("Param");

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
     * {@code FunctionalNamedArgument:  (UnionType | "function" | "void") MemberName Params+ (Block | NamedArguments)	 }
     */
    public static final Rule FunctionalNamedArgument = rule("FunctionalNamedArgument").any(UnionType, FUNCTION_MODIFIER, VOID_MODIFIER).one(MemberName).oneOrMore(Params).any(Block, NamedArguments);

    /**
     * {@code Object:  Annotation* ObjectHeader ClassBody	 }
     */
    public static final Rule Object = new DummyRule("Object");

    /**
     * {@code NamedArgument:  SpecifiedNamedArgument | LocalNamedArgument | FunctionalNamedArgument | Object	 }
     */
    public static final Rule NamedArgument = any(SpecifiedNamedArgument, LocalNamedArgument, FunctionalNamedArgument, Object);

    /**
     * {@code Sequence:  Expression ("," Expression)* | Expression "..."	 }
     */
    public static final Rule Sequence = new DummyRule("Sequence");

    /**
     * {@code Arguments:  PositionalArguments FunctionalArguments? | NamedArguments }
     */
    public static final Rule Arguments = rule("Arguments").any(sequence(PositionalArguments, zeroOrOne(FunctionalArguments)), NamedArguments);

    /**
     * {@code Annotation:  MemberName ( Arguments | Literal+ )?	 }
     */
    public static final Rule Annotation = rule("Annotation").one(MemberName).zeroOrAny(Arguments, oneOrMore(Literal));

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
    public static final Rule AbstractedType = new NotImplementedRule();

    /**
     * {@code AdaptedTypes:  "adapts" Type ("&" Type)*	 }
     */
    public static final Rule AdaptedTypes = new NotImplementedRule();

    /**
     * {@code Assignment:  ":=" | ".=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^="| "~=" | "&&=" | "||=" ;	 }
     */
    public static final Rule Assignment = new NotImplementedRule();

    /**
     * {@code AttributeMeta:  Type "." MemberName	 }
     */
    public static final Rule AttributeMeta = new NotImplementedRule();

    /**
     * {@code BooleanCondition:  Expression }
     */
    public static final Rule BooleanCondition = new NotImplementedRule();

    /**
     * {@code Break:  "break" }
     */
    public static final Rule Break = new NotImplementedRule();

    /**
     * {@code CallableReference:  MethodReference | InitializerReference	 }
     */
    public static final Rule CallableReference = new NotImplementedRule();

    /**
     * {@code CallableVariable:  (UnionType | "void")? MemberName Params+	 }
     */
    public static final Rule CallableVariable = new NotImplementedRule();

    /**
     * {@code Case:  Expression ("," Expression)* | "is" UnionType | "satisfies" Type	 }
     */
    public static final Rule Case = new NotImplementedRule();

    /**
     * {@code CaseItem:  "case" "(" Case ")" Block	 }
     */
    public static final Rule CaseItem = new NotImplementedRule();

    /**
     * {@code Cases:  CaseItem+ DefaultCaseItem?	 }
     */
    public static final Rule Cases = new NotImplementedRule();

    /**
     * {@code CaseType:  MemberName | Type	 }
     */
    public static final Rule CaseType = new NotImplementedRule();

    /**
     * {@code CaseTypes:  "of" CaseType ("|" CaseType)*	 }
     */
    public static final Rule CaseTypes = new NotImplementedRule();

    /**
     * {@code Catch:  "catch" "(" Variable ")" Block	 }
     */
    public static final Rule Catch = new NotImplementedRule();

    /**
     * {@code Character:  ~("`" | "\" | Tab | Formfeed | Newline | Return | Backspace) | EscapeSequence	 }
     */
    public static final Rule Character = new NotImplementedRule();

    /**
     * {@code Class:  Annotation* ClassHeader (ClassBody | TypeSpecifier ";")	 }
     */
    public static final Rule Class = new NotImplementedRule();

    /**
     * {@code ClassBody:  "{" (Declaration | Statement)* "}"	 }
     */
    public static final Rule ClassBody = new NotImplementedRule();

    /**
     * {@code ClassHeader:  "class" TypeName TypeParams? Params ClassInheritance TypeConstraints?	 }
     */
    public static final Rule ClassHeader = new NotImplementedRule();

    /**
     * {@code ClassInheritance:  CaseTypes? Metatypes? ExtendedType? SatisfiedTypes?	 }
     */
    public static final Rule ClassInheritance = new NotImplementedRule();

    /**
     * {@code ConcreteType:  "this" "is"	 }
     */
    public static final Rule ConcreteType = new NotImplementedRule();

    /**
     * {@code ConditionalTypes:  SatisfiedTypes Conditions	 }
     */
    public static final Rule ConditionalTypes = new NotImplementedRule();

    /**
     * {@code Condition:  BooleanCondition | IsCondition | ExistsOrNonemptyCondition | SatisfiesCondition	 }
     */
    public static final Rule Condition = new NotImplementedRule();

    /**
     * {@code Conditions:  "if" "(" Condition ("&&" Condition)* ")"	 }
     */
    public static final Rule Conditions = new NotImplementedRule();

    /**
     * {@code Continue:  "continue"	 }
     */
    public static final Rule Continue = new NotImplementedRule();

    /**
     * {@code ControlStructure:  IfElse | SwitchCaseElse | While | ForFail | TryCatchFinally	 }
     */
    public static final Rule ControlStructure = new NotImplementedRule();

    /**
     * {@code DateLiteral:   "'"  Digit{1,2} "/" Digit{1,2} "/" Digit{4}  "'"	 }
     */
    public static final Rule DateLiteral = new NotImplementedRule();

    /**
     * {@code DefaultCaseItem:  "else" Block	 }
     */
    public static final Rule DefaultCaseItem = new NotImplementedRule();

    /**
     * {@code Digit:  "0".."9"	 }
     */
    public static final Rule Digit = new NotImplementedRule();

    /**
     * {@code Digits:  Digit+ | Digit{1..3} ("_" Digit{3})+	 }
     */
    public static final Rule Digits = new NotImplementedRule();

    /**
     * {@code DimensionAtom:  DimensionConstant | DimensionVariable | ParenDimension	 }
     */
    public static final Rule DimensionAtom = new NotImplementedRule();

    /**
     * {@code DimensionConstant:  "#" IntegerLiteral	 }
     */
    public static final Rule DimensionConstant = new NotImplementedRule();

    /**
     * {@code Dimension:  DimensionTerm ("+" DimensionTerm)*	 }
     */
    public static final Rule Dimension = new NotImplementedRule();

    /**
     * {@code DimensionTerm:  (DimensionConstant "*")* DimensionAtom	 }
     */
    public static final Rule DimensionTerm = new NotImplementedRule();

    /**
     * {@code DimensionVariable:  TypeName | "#" MemberName	 }
     */
    public static final Rule DimensionVariable = new NotImplementedRule();

    /**
     * {@code Directive:  Return | Throw | Break | Continue	 }
     */
    public static final Rule Directive = new NotImplementedRule();

    /**
     * {@code DirectiveStatement:  Directive ";"	 }
     */
    public static final Rule DirectiveStatement = new NotImplementedRule();

    /**
     * {@code Else:  "else" (Block | IfElse)	 }
     */
    public static final Rule Else = new NotImplementedRule();

    /**
     * {@code EntryVariablePair:  Variable "->" Variable	 }
     */
    public static final Rule EntryVariablePair = new NotImplementedRule();

    /**
     * {@code EscapeSequence:  "\" ("b" | "t" | "n" | "f" | "r" | "\" | "\"" | "'" | "`" )	 }
     */
    public static final Rule EscapeSequence = new NotImplementedRule();

    /**
     * {@code ExistsOrNonemptyCondition:  ("exists" | "nonempty") (Variable Specifier | MemberName)	 }
     */
    public static final Rule ExistsOrNonemptyCondition = new NotImplementedRule();

    /**
     * {@code Exponent:  ("E"|"e") ("+"|"-")? Digits	 }
     */
    public static final Rule Exponent = new NotImplementedRule();

    /**
     * {@code ExpressionStatement:  ( Assignment | IncrementOrDecrement | Invocation ) ";"	 }
     */
    public static final Rule ExpressionStatement = new NotImplementedRule();

    /**
     * {@code ExtendedType:  "extends" ("super" ".")? Type PositionalArguments	 }
     */
    public static final Rule ExtendedType = new NotImplementedRule();

    /**
     * {@code Fail:  "else" Block	 }
     */
    public static final Rule Fail = new NotImplementedRule();

    /**
     * {@code Finally:  "finally" Block	 }
     */
    public static final Rule Finally = new NotImplementedRule();

    /**
     * {@code ForFail:  For Fail?	 }
     */
    public static final Rule ForFail = new NotImplementedRule();

    /**
     * {@code For:  "for" "(" ForIterator ")" Block	 }
     */
    public static final Rule For = new NotImplementedRule();

    /**
     * {@code ForIterator:  IteratorVariable "in" Expression	 }
     */
    public static final Rule ForIterator = new NotImplementedRule();

    /**
     * {@code FractionalDigits:  Digit+ | (Digit{3} "_")+ Digit{1..3}	 }
     */
    public static final Rule FractionalDigits = new NotImplementedRule();

    /**
     * {@code FractionalMagnitude:  "m" | "u" | "n" | "p" | "f"	 }
     */
    public static final Rule FractionalMagnitude = new NotImplementedRule();

    /**
     * {@code FunctionalBody:  Params? ( Block | "(" Expression ")" )	 }
     */
    public static final Rule FunctionalBody = new NotImplementedRule();

    /**
     * {@code FunctionMeta:  MemberName TypeArguments?	 }
     */
    public static final Rule FunctionMeta = new NotImplementedRule();

    /**
     * {@code IdentifierChar:  LowercaseChar | UppercaseChar | Digit	 }
     */
    public static final Rule IdentifierChar = new NotImplementedRule();

    /**
     * {@code IfElse:  If Else?	 }
     */
    public static final Rule IfElse = new NotImplementedRule();

    /**
     * {@code If:  "if" "(" Condition ")" Block	 }
     */
    public static final Rule If = new NotImplementedRule();

    /**
     * {@code IncrementOrDecrement:  "--" | "++" ; }
     */
    public static final Rule IncrementOrDecrement = new NotImplementedRule();

    /**
     * {@code Initializer:  ":=" Expression	 }
     */
    public static final Rule Initializer = new NotImplementedRule();

    /**
     * {@code InitializerReference:  (Receiver ".")? TypeName TypeArguments?	 }
     */
    public static final Rule InitializerReference = new NotImplementedRule();

    /**
     * {@code Interface:  Annotation* InterfaceHeader (InterfaceBody | TypeSpecifier ";")	 }
     */
    public static final Rule Interface = new NotImplementedRule();

    /**
     * {@code InterfaceBody:  "{" Declaration* "}"	 }
     */
    public static final Rule InterfaceBody = new NotImplementedRule();

    /**
     * {@code InterfaceHeader:  "interface" TypeName TypeParams? InterfaceInheritance TypeConstraints?	 }
     */
    public static final Rule InterfaceHeader = new NotImplementedRule();

    /**
     * {@code InterfaceInheritance:  CaseTypes? Metatypes? AdaptedTypes? SatisfiedTypes?	 }
     */
    public static final Rule InterfaceInheritance = new NotImplementedRule();

    /**
     * {@code Introduction:  "adapt" Type SatisfiedTypes TypeConstraints? ";"	 }
     */
    public static final Rule Introduction = new NotImplementedRule();

    /**
     * {@code IsCondition:  "is" (TypedVariable Specifier | UnionType MemberName)	 }
     */
    public static final Rule IsCondition = new NotImplementedRule();

    /**
     * {@code IteratorVariable:  Variable | CallableVariable | EntryVariablePair	 }
     */
    public static final Rule IteratorVariable = new NotImplementedRule();

    /**
     * {@code LineComment:  ("//"|"#!") ~(Newline|Return)* (Return Newline | Return | Newline)?	 }
     */
    public static final Rule LineComment = new NotImplementedRule();

    /**
     * {@code LoopCondition:  "while" "(" Condition ")"	 }
     */
    public static final Rule LoopCondition = new NotImplementedRule();

    /**
     * {@code LowercaseChar:  "a".."z" | "_" ;	 }
     */
    public static final Rule LowercaseChar = new NotImplementedRule();

    /**
     * {@code Magnitude:  "k" | "M" | "G" | "T" | "P"	 }
     */
    public static final Rule Magnitude = new NotImplementedRule();

    /**
     * {@code Metatypes:  "is" Type ("&" Type)*	 }
     */
    public static final Rule Metatypes = new NotImplementedRule();

    /**
     * {@code MethodHeader:  (UnionType | "function" | "void") MemberName TypeParams? Params+ Metatypes? TypeConstraints?	 }
     */
    public static final Rule MethodHeader = new NotImplementedRule();

    /**
     * {@code MethodMeta:  Type "." MemberName TypeArguments?	 }
     */
    public static final Rule MethodMeta = new NotImplementedRule();

    /**
     * {@code MethodReference:  (Receiver ".")? MemberName TypeArguments?	 }
     */
    public static final Rule MethodReference = new NotImplementedRule();

    /**
     * {@code MultilineComment:  "/" "*" ( MultilineCommmentCharacter | MultilineComment )* "*" "/"	 }
     */
    public static final Rule MultilineComment = new NotImplementedRule();

    /**
     * {@code MultilineCommmentCharacter:  ~("/"|"*") | ("/" ~"*") => "/" | ("*" ~"/") => "*"	 }
     */
    public static final Rule MultilineCommmentCharacter = new NotImplementedRule();

    /**
     * {@code ObjectHeader:  "object" MemberName ObjectInheritance	 }
     */
    public static final Rule ObjectHeader = new NotImplementedRule();

    /**
     * {@code ObjectInheritance:  ExtendedType? SatisfiedTypes?	 }
     */
    public static final Rule ObjectInheritance = new NotImplementedRule();

    /**
     * {@code OuterReference:  (Receiver ".")? "outer"	 }
     */
    public static final Rule OuterReference = new NotImplementedRule();

    /**
     * {@code ParenDimension:  "(" Dimension ")"	 }
     */
    public static final Rule ParenDimension = new NotImplementedRule();

    /**
     * {@code QuotedLiteralCharacter:  ~("'")	 }
     */
    public static final Rule QuotedLiteralCharacter = new NotImplementedRule();

    /**
     * {@code Receiver:  Primary	 }
     */
    public static final Rule Receiver = new NotImplementedRule();

    /**
     * {@code Resource:  MemberName | InitializerReference Arguments | Variable Specifier	 }
     */
    public static final Rule Resource = new NotImplementedRule();

    /**
     * {@code Retry:  "retry"	 }
     */
    public static final Rule Retry = new NotImplementedRule();

    /**
     * {@code Return:  "return" Expression?	 }
     */
    public static final Rule Return = new NotImplementedRule();

    /**
     * {@code SatisfiedTypes:  "satisfies" Type ("&" Type)*	 }
     */
    public static final Rule SatisfiedTypes = new NotImplementedRule();

    /**
     * {@code SatisfiesCondition:  "satisfies" Type Type	 }
     */
    public static final Rule SatisfiesCondition = new NotImplementedRule();

    /**
     * {@code SequencedTypeParam:  TypeName "..."	 }
     */
    public static final Rule SequencedTypeParam = new NotImplementedRule();

    /**
     * {@code SequencedType:  TypeName "..."	 }
     */
    public static final Rule SequencedType = new NotImplementedRule();

    /**
     * {@code SequenceInstantiation:  "{" Sequence? "}" ;	 }
     */
    public static final Rule SequenceInstantiation = new NotImplementedRule();

    /**
     * {@code Specification:  MemberName Specifier ";"	 }
     */
    public static final Rule Specification = new NotImplementedRule();

    /**
     * {@code Statement:  ExpressionStatement | Specification | DirectiveStatement | ControlStructure	 }
     */
    public static final Rule Statement = new DummyRule("Statement");

    /**
     * {@code StringCharacter:  ~( "\" | "\"" ) | EscapeSequence	 }
     */
    public static final Rule StringCharacter = new NotImplementedRule();

    /**
     * {@code Subtype:  "subtype" | MemberName "." "subtype"	 }
     */
    public static final Rule Subtype = new NotImplementedRule();

    /**
     * {@code SwitchCaseElse:  Switch ( Cases | "{" Cases "}" )	 }
     */
    public static final Rule SwitchCaseElse = new NotImplementedRule();

    /**
     * {@code Switch:  "switch" "(" Expression ")"	 }
     */
    public static final Rule Switch = new NotImplementedRule();

    /**
     * {@code Throw:  "throw" Expression?	 }
     */
    public static final Rule Throw = new NotImplementedRule();

    /**
     * {@code TimeLiteral:   "'"  Digit{1,2} ":" Digit{2} ( ":" Digit{2} ( ":" Digit{3} )? )?  (" " "AM"|"PM")?  (" " Character{3,4})?  "'"	 }
     */
    public static final Rule TimeLiteral = new NotImplementedRule();

    /**
     * {@code TryCatchFinally:  Try Catch* Finally?	 }
     */
    public static final Rule TryCatchFinally = new NotImplementedRule();

    /**
     * {@code Try:  "try" ("(" Resource ")")? Block	 }
     */
    public static final Rule Try = new NotImplementedRule();

    /**
     * {@code TypeArgument:  UnionType | Dimension	 }
     */
    public static final Rule TypeArgument = new NotImplementedRule();

    /**
     * {@code TypeConstraint:  "given" TypeName TypeParams? Params? TypeConstraintInheritance	 }
     */
    public static final Rule TypeConstraint = new NotImplementedRule();

    /**
     * {@code TypeConstraintInheritance:  CaseTypes? Metatypes? SatisfiedTypes? AbstractedType?	 }
     */
    public static final Rule TypeConstraintInheritance = new NotImplementedRule();

    /**
     * {@code TypeConstraints:  TypeConstraint+	 }
     */
    public static final Rule TypeConstraints = new NotImplementedRule();

    /**
     * {@code TypedQuotedLiteral:  TypeName QuotedLiteral	 }
     */
    public static final Rule TypedQuotedLiteral = new NotImplementedRule();

    /**
     * {@code TypedVariable:  UnionType MemberName	 }
     */
    public static final Rule TypedVariable = new NotImplementedRule();

    /**
     * {@code TypeMeta:  Type	 }
     */
    public static final Rule TypeMeta = new NotImplementedRule();

    /**
     * {@code TypeParams:  "<" (TypeParam ",")* (TypeParam | SequencedTypeParam) ">"	 }
     */
    public static final Rule TypeParams = new NotImplementedRule();

    /**
     * {@code TypeParam:  Variance? TypeName	 }
     */
    public static final Rule TypeParam = new NotImplementedRule();

    /**
     * {@code TypeSpecifier:  "=" Type	 }
     */
    public static final Rule TypeSpecifier = new NotImplementedRule();

    /**
     * {@code UppercaseChar:  "A".."Z" ;	 }
     */
    public static final Rule UppercaseChar = new NotImplementedRule();

    /**
     * {@code ValueMeta:  MemberName TypeArguments?	 }
     */
    public static final Rule ValueMeta = new NotImplementedRule();

    /**
     * {@code ValueReference:  (Receiver ".")? MemberName	 }
     */
    public static final Rule ValueReference = new NotImplementedRule();

    /**
     * {@code Variable:  UnionType? MemberName	 }
     */
    public static final Rule Variable = new NotImplementedRule();

    /**
     * {@code Variance:  "out" | "in"	 }
     */
    public static final Rule Variance = new NotImplementedRule();

    /**
     * {@code While:  LoopCondition Block	 }
     */
    public static final Rule While = new NotImplementedRule();

    /**
     * {@code Whitespace:  " " | Tab | Formfeed | Newline | Return	 }
     */
    public static final Rule Whitespace = new NotImplementedRule();

    static {
        Block.one(LBRACE).zeroOrAny(Declaration, Statement).one(RBRACE);
        Expression.any(Primary, OperatorExpression);
        NamedArguments.one(LBRACE).zeroOrMore(NamedArgument).zeroOrOne(Sequence).one(RBRACE);
        Param.zeroOrMore(Annotation).any(SimpleParam, CallableParam, EntryParamPair);

    }

}
