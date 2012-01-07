package org.ceylon.idea.lang.parser.rule;

import com.intellij.lang.PsiBuilder;

import static org.ceylon.idea.lang.lexer.CeylonToken.*;
import static org.ceylon.idea.lang.parser.rule.AnyRule.any;
import static org.ceylon.idea.lang.parser.rule.ComplexRule.rule;
import static org.ceylon.idea.lang.parser.rule.SequenceRule.sequence;
import static org.ceylon.idea.lang.parser.rule.ZeroOrMoreRule.zeroOrMore;
import static org.ceylon.idea.lang.parser.rule.ZeroOrOneRule.zeroOrOne;

@SuppressWarnings("unused")
public interface Rule {

    /**
     * {@code AnnotationName:  LIdentifier }
     */
    Rule AnnotationName = rule("AnnotationName").one(LIDENTIFIER);

    /**
     * {@code CompilerAnnotation : "@" AnnotationName ("[" StringLiteral "]")? }
     */
    Rule CompilerAnnotation = rule("CompilerAnnotation").sequence(COMPILER_ANNOTATION, AnnotationName).zeroOrOne(INDEX_OP, STRING_LITERAL, RBRACKET);

    /**
     * {@code CompilerAnnotations: CompilerAnnotation* }
     */
    Rule CompilerAnnotations = zeroOrMore(CompilerAnnotation);

    /**
     * {@code PackageName:  PIdentifier }
     */
    Rule PackageName = rule("PackageName").one(LIDENTIFIER);

    /**
     * {@code FullPackageName:  PackageName ("." PackageName)* }
     */
    Rule FullPackageName = rule("FullPackageName").one(PackageName).zeroOrMore(MEMBER_OP, PackageName);

    /**
     * {@code ImportWildcard:  "..." }
     */
    Rule ImportWildcard = rule("ImportWildcard").one(ELLIPSIS);

    /**
     * {@code TypeName:  UIdentifier }
     */
    Rule TypeName = rule("TypeName").one(UIDENTIFIER);

    /**
     * {@code TypeAlias:  TypeName "=" }
     */
    Rule TypeAlias = rule("TypeAlias").sequence(TypeName, EQUAL_OP);

    /**
     * {@code ImportTypeElement:  TypeAlias? TypeName }
     */
    Rule ImportTypeElement = rule("ImportTypeElement").zeroOrOne(TypeAlias).one(TypeName);

    /**
     * {@code MemberName:  LIdentifier	 }
     */
    Rule MemberName = rule("MemberName").one(LIDENTIFIER);

    /**
     * {@code MethodAttributeAlias:  MemberName "="	 }
     */
    Rule MethodAttributeAlias = rule("MethodAttributeAlias").sequence(MemberName, EQUAL_OP);

    /**
     * {@code ImportMethodAttributeElement:  MethodAttributeAlias? MemberName }
     */
    Rule ImportMethodAttributeElement = rule("ImportMethodAttributeElement").zeroOrOne(MethodAttributeAlias).one(MemberName);

    /**
     * {@code ImportElement:  ImportTypeElement | ImportMethodAttributeElement }
     */
    Rule ImportElement = any(ImportTypeElement, ImportMethodAttributeElement);

    /**
     * {@code ImportElements:  ImportElement ("," ImportElement)* ("," ImportWildcard)? | ImportWildcard }
     */
    Rule ImportElements = rule("ImportElements")
            .any(
                    sequence(ImportElement, zeroOrMore(COMMA, ImportElement), zeroOrOne(COMMA, ImportWildcard)),
                    ImportWildcard);

    /**
     * {@code Import:  "import" FullPackageName "{" ImportElements? "}" }
     */
    Rule Import = rule("Import").sequence(IMPORT, FullPackageName).one(LBRACE).zeroOrOne(ImportElements).one(RBRACE);

    /**
     * {@code ToplevelDeclaration:  TypeDeclaration | Method | SimpleAttribute | AttributeGetter }
     * {@code Declaration:  Method | Attribute | TypeDeclaration }
     */
    Rule Declaration = new DummyRule("Declaration");

    /**
     * {@code CompilationUnit : (CompilerAnnotation+ ";")? import* (CompilerAnnotations Declaration)* EOF }
     */
    Rule CompilationUnit = rule("CompilationUnit").zeroOrOne(CompilerAnnotation, CompilerAnnotations, SEMICOLON).zeroOrMore(Import).zeroOrMore(CompilerAnnotations, Declaration);


    /**
     * {@code AbbreviatedType:  Type Abbreviation* }
     */
    Rule AbbreviatedType = new NotImplementedRule();

    /**
     * {@code Abbreviation:  "?" | "[]"	 }
     */
    Rule Abbreviation = new NotImplementedRule();

    /**
     * {@code AbstractedType:  "abstracts" Type	 }
     */
    Rule AbstractedType = new NotImplementedRule();

    /**
     * {@code AdaptedTypes:  "adapts" Type ("&" Type)*	 }
     */
    Rule AdaptedTypes = new NotImplementedRule();

    /**
     * {@code Annotation:  MemberName ( Arguments | Literal+ )?	 }
     */
    Rule Annotation = new NotImplementedRule();

    /**
     * {@code Arguments:  PositionalArguments FunctionalArguments? | NamedArguments	 }
     */
    Rule Arguments = new NotImplementedRule();

    /**
     * {@code Assignment:  ":=" | ".=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^="| "~=" | "&&=" | "||=" ;	 }
     */
    Rule Assignment = new NotImplementedRule();

    /**
     * {@code Atom:  Literal | StringTemplate | SelfReference | ParExpression	 }
     */
    Rule Atom = new NotImplementedRule();

    /**
     * {@code Attribute:  Annotation* (SimpleAttribute | AttributeGetter | AttributeSetter)	 }
     */
    Rule Attribute = new NotImplementedRule();

    /**
     * {@code AttributeGetter:  AttributeHeader Block	 }
     */
    Rule AttributeGetter = new NotImplementedRule();

    /**
     * {@code AttributeHeader:  (UnionType | "value") MemberName	 }
     */
    Rule AttributeHeader = new NotImplementedRule();

    /**
     * {@code AttributeMeta:  Type "." MemberName	 }
     */
    Rule AttributeMeta = new NotImplementedRule();

    /**
     * {@code AttributeSetter:  "assign" MemberName Block	 }
     */
    Rule AttributeSetter = new NotImplementedRule();

    /**
     * {@code Block:  "{" (Declaration | Statement)* "}"	 }
     */
    Rule Block = new NotImplementedRule();

    /**
     * {@code BooleanCondition:  Expression	 }
     */
    Rule BooleanCondition = new NotImplementedRule();

    /**
     * {@code Break:  "break"	 }
     */
    Rule Break = new NotImplementedRule();

    /**
     * {@code CallableParam:  (UnionType | "void") MemberName Params+	 }
     */
    Rule CallableParam = new NotImplementedRule();

    /**
     * {@code CallableReference:  MethodReference | InitializerReference	 }
     */
    Rule CallableReference = new NotImplementedRule();

    /**
     * {@code CallableVariable:  (UnionType | "void")? MemberName Params+	 }
     */
    Rule CallableVariable = new NotImplementedRule();

    /**
     * {@code Case:  Expression ("," Expression)* | "is" UnionType | "satisfies" Type	 }
     */
    Rule Case = new NotImplementedRule();

    /**
     * {@code CaseItem:  "case" "(" Case ")" Block	 }
     */
    Rule CaseItem = new NotImplementedRule();

    /**
     * {@code Cases:  CaseItem+ DefaultCaseItem?	 }
     */
    Rule Cases = new NotImplementedRule();

    /**
     * {@code CaseType:  MemberName | Type	 }
     */
    Rule CaseType = new NotImplementedRule();

    /**
     * {@code CaseTypes:  "of" CaseType ("|" CaseType)*	 }
     */
    Rule CaseTypes = new NotImplementedRule();

    /**
     * {@code Catch:  "catch" "(" Variable ")" Block	 }
     */
    Rule Catch = new NotImplementedRule();

    /**
     * {@code CharacterLiteral:  "`" Character "`"	 }
     */
    Rule CharacterLiteral = new NotImplementedRule();

    /**
     * {@code Character:  ~("`" | "\" | Tab | Formfeed | Newline | Return | Backspace) | EscapeSequence	 }
     */
    Rule Character = new NotImplementedRule();

    /**
     * {@code Class:  Annotation* ClassHeader (ClassBody | TypeSpecifier ";")	 }
     */
    Rule Class = new NotImplementedRule();

    /**
     * {@code ClassBody:  "{" (Declaration | Statement)* "}"	 }
     */
    Rule ClassBody = new NotImplementedRule();

    /**
     * {@code ClassHeader:  "class" TypeName TypeParams? Params ClassInheritance TypeConstraints?	 }
     */
    Rule ClassHeader = new NotImplementedRule();

    /**
     * {@code ClassInheritance:  CaseTypes? Metatypes? ExtendedType? SatisfiedTypes?	 }
     */
    Rule ClassInheritance = new NotImplementedRule();

    /**
     * {@code ConcreteType:  "this" "is"	 }
     */
    Rule ConcreteType = new NotImplementedRule();

    /**
     * {@code ConditionalTypes:  SatisfiedTypes Conditions	 }
     */
    Rule ConditionalTypes = new NotImplementedRule();

    /**
     * {@code Condition:  BooleanCondition | IsCondition | ExistsOrNonemptyCondition | SatisfiesCondition	 }
     */
    Rule Condition = new NotImplementedRule();

    /**
     * {@code Conditions:  "if" "(" Condition ("&&" Condition)* ")"	 }
     */
    Rule Conditions = new NotImplementedRule();

    /**
     * {@code Continue:  "continue"	 }
     */
    Rule Continue = new NotImplementedRule();

    /**
     * {@code ControlStructure:  IfElse | SwitchCaseElse | While | ForFail | TryCatchFinally	 }
     */
    Rule ControlStructure = new NotImplementedRule();

    /**
     * {@code DateLiteral:   "'"  Digit{1,2} "/" Digit{1,2} "/" Digit{4}  "'"	 }
     */
    Rule DateLiteral = new NotImplementedRule();

    /**
     * {@code DefaultCaseItem:  "else" Block	 }
     */
    Rule DefaultCaseItem = new NotImplementedRule();

    /**
     * {@code DefaultParam:  Param Specifier	 }
     */
    Rule DefaultParam = new NotImplementedRule();

    /**
     * {@code Digit:  "0".."9"	 }
     */
    Rule Digit = new NotImplementedRule();

    /**
     * {@code Digits:  Digit+ | Digit{1..3} ("_" Digit{3})+	 }
     */
    Rule Digits = new NotImplementedRule();

    /**
     * {@code DimensionAtom:  DimensionConstant | DimensionVariable | ParenDimension	 }
     */
    Rule DimensionAtom = new NotImplementedRule();

    /**
     * {@code DimensionConstant:  "#" IntegerLiteral	 }
     */
    Rule DimensionConstant = new NotImplementedRule();

    /**
     * {@code Dimension:  DimensionTerm ("+" DimensionTerm)*	 }
     */
    Rule Dimension = new NotImplementedRule();

    /**
     * {@code DimensionTerm:  (DimensionConstant "*")* DimensionAtom	 }
     */
    Rule DimensionTerm = new NotImplementedRule();

    /**
     * {@code DimensionVariable:  TypeName | "#" MemberName	 }
     */
    Rule DimensionVariable = new NotImplementedRule();

    /**
     * {@code Directive:  Return | Throw | Break | Continue	 }
     */
    Rule Directive = new NotImplementedRule();

    /**
     * {@code DirectiveStatement:  Directive ";"	 }
     */
    Rule DirectiveStatement = new NotImplementedRule();

    /**
     * {@code Else:  "else" (Block | IfElse)	 }
     */
    Rule Else = new NotImplementedRule();

    /**
     * {@code EntryParamPair:  SimpleParam "->" SimpleParam	 }
     */
    Rule EntryParamPair = new NotImplementedRule();

    /**
     * {@code EntryType:  AbbreviatedType ("->" AbbreviatedType)?	 }
     */
    Rule EntryType = new NotImplementedRule();

    /**
     * {@code EntryVariablePair:  Variable "->" Variable	 }
     */
    Rule EntryVariablePair = new NotImplementedRule();

    /**
     * {@code EscapeSequence:  "\" ("b" | "t" | "n" | "f" | "r" | "\" | "\"" | "'" | "`" )	 }
     */
    Rule EscapeSequence = new NotImplementedRule();

    /**
     * {@code ExistsOrNonemptyCondition:  ("exists" | "nonempty") (Variable Specifier | MemberName)	 }
     */
    Rule ExistsOrNonemptyCondition = new NotImplementedRule();

    /**
     * {@code Exponent:  ("E"|"e") ("+"|"-")? Digits	 }
     */
    Rule Exponent = new NotImplementedRule();

    /**
     * {@code Expression:  Primary | OperatorExpression	 }
     */
    Rule Expression = new NotImplementedRule();

    /**
     * {@code ExpressionStatement:  ( Assignment | IncrementOrDecrement | Invocation ) ";"	 }
     */
    Rule ExpressionStatement = new NotImplementedRule();

    /**
     * {@code ExtendedType:  "extends" ("super" ".")? Type PositionalArguments	 }
     */
    Rule ExtendedType = new NotImplementedRule();

    /**
     * {@code Fail:  "else" Block	 }
     */
    Rule Fail = new NotImplementedRule();

    /**
     * {@code Finally:  "finally" Block	 }
     */
    Rule Finally = new NotImplementedRule();

    /**
     * {@code FloatLiteral:  Digits ("." FractionalDigits (Exponent | Magnitude | FractionalMagnitude)? | FractionalMagnitude)	 }
     */
    Rule FloatLiteral = new NotImplementedRule();

    /**
     * {@code ForFail:  For Fail?	 }
     */
    Rule ForFail = new NotImplementedRule();

    /**
     * {@code For:  "for" "(" ForIterator ")" Block	 }
     */
    Rule For = new NotImplementedRule();

    /**
     * {@code ForIterator:  IteratorVariable "in" Expression	 }
     */
    Rule ForIterator = new NotImplementedRule();

    /**
     * {@code FractionalDigits:  Digit+ | (Digit{3} "_")+ Digit{1..3}	 }
     */
    Rule FractionalDigits = new NotImplementedRule();

    /**
     * {@code FractionalMagnitude:  "m" | "u" | "n" | "p" | "f"	 }
     */
    Rule FractionalMagnitude = new NotImplementedRule();

    /**
     * {@code FunctionalArguments:  (MemberName FunctionalBody)+	 }
     */
    Rule FunctionalArguments = new NotImplementedRule();

    /**
     * {@code FunctionalBody:  Params? ( Block | "(" Expression ")" )	 }
     */
    Rule FunctionalBody = new NotImplementedRule();

    /**
     * {@code FunctionalNamedArgument:  (UnionType | "function" | "void") MemberName Params+ (Block | NamedArguments)	 }
     */
    Rule FunctionalNamedArgument = new NotImplementedRule();

    /**
     * {@code FunctionMeta:  MemberName TypeArguments?	 }
     */
    Rule FunctionMeta = new NotImplementedRule();

    /**
     * {@code IdentifierChar:  LowercaseChar | UppercaseChar | Digit	 }
     */
    Rule IdentifierChar = new NotImplementedRule();

    /**
     * {@code IfElse:  If Else?	 }
     */
    Rule IfElse = new NotImplementedRule();

    /**
     * {@code If:  "if" "(" Condition ")" Block	 }
     */
    Rule If = new NotImplementedRule();

    /**
     * {@code IncrementOrDecrement:  "--" | "++" ; }
     */
    Rule IncrementOrDecrement = new NotImplementedRule();

    /**
     * {@code Initializer:  ":=" Expression	 }
     */
    Rule Initializer = new NotImplementedRule();

    /**
     * {@code InitializerReference:  (Receiver ".")? TypeName TypeArguments?	 }
     */
    Rule InitializerReference = new NotImplementedRule();

    /**
     * {@code IntegerLiteral:  Digits Magnitude?	 }
     */
    Rule IntegerLiteral = new NotImplementedRule();

    /**
     * {@code Interface:  Annotation* InterfaceHeader (InterfaceBody | TypeSpecifier ";")	 }
     */
    Rule Interface = new NotImplementedRule();

    /**
     * {@code InterfaceBody:  "{" Declaration* "}"	 }
     */
    Rule InterfaceBody = new NotImplementedRule();

    /**
     * {@code InterfaceHeader:  "interface" TypeName TypeParams? InterfaceInheritance TypeConstraints?	 }
     */
    Rule InterfaceHeader = new NotImplementedRule();

    /**
     * {@code InterfaceInheritance:  CaseTypes? Metatypes? AdaptedTypes? SatisfiedTypes?	 }
     */
    Rule InterfaceInheritance = new NotImplementedRule();

    /**
     * {@code IntersectionType:  EntryType ("&" EntryType)*	 }
     */
    Rule IntersectionType = new NotImplementedRule();

    /**
     * {@code Introduction:  "adapt" Type SatisfiedTypes TypeConstraints? ";"	 }
     */
    Rule Introduction = new NotImplementedRule();

    /**
     * {@code Invocation:  Primary Arguments | SequenceInstantiation	 }
     */
    Rule Invocation = new NotImplementedRule();

    /**
     * {@code IsCondition:  "is" (TypedVariable Specifier | UnionType MemberName)	 }
     */
    Rule IsCondition = new NotImplementedRule();

    /**
     * {@code IteratorVariable:  Variable | CallableVariable | EntryVariablePair	 }
     */
    Rule IteratorVariable = new NotImplementedRule();

    /**
     * {@code LineComment:  ("//"|"#!") ~(Newline|Return)* (Return Newline | Return | Newline)?	 }
     */
    Rule LineComment = new NotImplementedRule();

    /**
     * {@code Literal:  IntegerLiteral | FloatLiteral | CharacterLiteral | StringLiteral | QuotedLiteral	 }
     */
    Rule Literal = new NotImplementedRule();

    /**
     * {@code LocalNamedArgument:  (UnionType | "value") MemberName (Block | NamedArguments)	 }
     */
    Rule LocalNamedArgument = new NotImplementedRule();

    /**
     * {@code LoopCondition:  "while" "(" Condition ")"	 }
     */
    Rule LoopCondition = new NotImplementedRule();

    /**
     * {@code LowercaseChar:  "a".."z" | "_" ;	 }
     */
    Rule LowercaseChar = new NotImplementedRule();

    /**
     * {@code Magnitude:  "k" | "M" | "G" | "T" | "P"	 }
     */
    Rule Magnitude = new NotImplementedRule();

    /**
     * {@code MemberReference:  CallableReference | ValueReference	 }
     */
    Rule MemberReference = new NotImplementedRule();

    /**
     * {@code Meta:  TypeMeta | MethodMeta | AttributeMeta | FunctionMeta | ValueMeta	 }
     */
    Rule Meta = new NotImplementedRule();

    /**
     * {@code Metatypes:  "is" Type ("&" Type)*	 }
     */
    Rule Metatypes = new NotImplementedRule();

    /**
     * {@code Method:  Annotation* MethodHeader (Block | NamedArguments | Specifier? ";")	 }
     */
    Rule Method = new NotImplementedRule();

    /**
     * {@code MethodHeader:  (UnionType | "function" | "void") MemberName TypeParams? Params+ Metatypes? TypeConstraints?	 }
     */
    Rule MethodHeader = new NotImplementedRule();

    /**
     * {@code MethodMeta:  Type "." MemberName TypeArguments?	 }
     */
    Rule MethodMeta = new NotImplementedRule();

    /**
     * {@code MethodReference:  (Receiver ".")? MemberName TypeArguments?	 }
     */
    Rule MethodReference = new NotImplementedRule();

    /**
     * {@code MultilineComment:  "/" "*" ( MultilineCommmentCharacter | MultilineComment )* "*" "/"	 }
     */
    Rule MultilineComment = new NotImplementedRule();

    /**
     * {@code MultilineCommmentCharacter:  ~("/"|"*") | ("/" ~"*") => "/" | ("*" ~"/") => "*"	 }
     */
    Rule MultilineCommmentCharacter = new NotImplementedRule();

    /**
     * {@code NamedArguments:  "{" NamedArgument* Sequence? "}"	 }
     */
    Rule NamedArguments = new NotImplementedRule();

    /**
     * {@code NamedArgument:  SpecifiedNamedArgument | LocalNamedArgument | FunctionalNamedArgument | Object	 }
     */
    Rule NamedArgument = new NotImplementedRule();

    /**
     * {@code Object:  Annotation* ObjectHeader ClassBody	 }
     */
    Rule Object = new NotImplementedRule();

    /**
     * {@code ObjectHeader:  "object" MemberName ObjectInheritance	 }
     */
    Rule ObjectHeader = new NotImplementedRule();

    /**
     * {@code ObjectInheritance:  ExtendedType? SatisfiedTypes?	 }
     */
    Rule ObjectInheritance = new NotImplementedRule();

    /**
     * {@code OuterReference:  (Receiver ".")? "outer"	 }
     */
    Rule OuterReference = new NotImplementedRule();

    /**
     * {@code Param:  Annotation* (SimpleParam | CallableParam | EntryParamPair) }
     */
    Rule Param = new NotImplementedRule();

    /**
     * {@code Params:   "(" Param ("," Param)* ("," DefaultParam)* ("," SequencedParam)? |  DefaultParam ("," DefaultParam)* ("," SequencedParam)? |  SequencedParam? ")"	 }
     */
    Rule Params = new NotImplementedRule();

    /**
     * {@code ParenDimension:  "(" Dimension ")"	 }
     */
    Rule ParenDimension = new NotImplementedRule();

    /**
     * {@code ParExpression:  "(" Expression ")"	 }
     */
    Rule ParExpression = new NotImplementedRule();

    /**
     * {@code PositionalArguments:  "(" Expression ("," Expression)* ("," Sequence)? | Sequence? ")" }
     */
    Rule PositionalArguments = new NotImplementedRule();

    /**
     * {@code Primary:  Atom | Meta | MemberReference | Invocation	 }
     */
    Rule Primary = new NotImplementedRule();

    /**
     * {@code QuotedLiteralCharacter:  ~("'")	 }
     */
    Rule QuotedLiteralCharacter = new NotImplementedRule();

    /**
     * {@code QuotedLiteral:  "'" QuotedLiteralCharacter* "'"	 }
     */
    Rule QuotedLiteral = new NotImplementedRule();

    /**
     * {@code Receiver:  Primary	 }
     */
    Rule Receiver = new NotImplementedRule();

    /**
     * {@code Resource:  MemberName | InitializerReference Arguments | Variable Specifier	 }
     */
    Rule Resource = new NotImplementedRule();

    /**
     * {@code Retry:  "retry"	 }
     */
    Rule Retry = new NotImplementedRule();

    /**
     * {@code Return:  "return" Expression?	 }
     */
    Rule Return = new NotImplementedRule();

    /**
     * {@code SatisfiedTypes:  "satisfies" Type ("&" Type)*	 }
     */
    Rule SatisfiedTypes = new NotImplementedRule();

    /**
     * {@code SatisfiesCondition:  "satisfies" Type Type	 }
     */
    Rule SatisfiesCondition = new NotImplementedRule();

    /**
     * {@code SelfReference:  "this" | "super" | "outer"	 }
     */
    Rule SelfReference = new NotImplementedRule();

    /**
     * {@code SequencedParam:  Annotation* UnionType "..." MemberName	 }
     */
    Rule SequencedParam = new NotImplementedRule();

    /**
     * {@code SequencedTypeParam:  TypeName "..."	 }
     */
    Rule SequencedTypeParam = new NotImplementedRule();

    /**
     * {@code SequencedType:  TypeName "..."	 }
     */
    Rule SequencedType = new NotImplementedRule();

    /**
     * {@code Sequence:  Expression ("," Expression)* | Expression "..."	 }
     */
    Rule Sequence = new NotImplementedRule();

    /**
     * {@code SequenceInstantiation:  "{" Sequence? "}" ;	 }
     */
    Rule SequenceInstantiation = new NotImplementedRule();

    /**
     * {@code SimpleAttribute:  AttributeHeader ( (Specifier | Initializer)? ";" | NamedArguments )	 }
     */
    Rule SimpleAttribute = new NotImplementedRule();

    /**
     * {@code SimpleParam:  UnionType MemberName	 }
     */
    Rule SimpleParam = new NotImplementedRule();

    /**
     * {@code Specification:  MemberName Specifier ";"	 }
     */
    Rule Specification = new NotImplementedRule();

    /**
     * {@code SpecifiedNamedArgument:  MemberName Specifier ";"	 }
     */
    Rule SpecifiedNamedArgument = new NotImplementedRule();

    /**
     * {@code Specifier:  "=" Expression	 }
     */
    Rule Specifier = new NotImplementedRule();

    /**
     * {@code Statement:  ExpressionStatement | Specification | DirectiveStatement | ControlStructure	 }
     */
    Rule Statement = new NotImplementedRule();

    /**
     * {@code StringCharacter:  ~( "\" | "\"" ) | EscapeSequence	 }
     */
    Rule StringCharacter = new NotImplementedRule();

    /**
     * {@code StringLiteral:  "\"" StringCharacter* "\""	 }
     */
    Rule StringLiteral = new NotImplementedRule();

    /**
     * {@code StringTemplate:  StringLiteral (Expression StringLiteral)+	 }
     */
    Rule StringTemplate = new NotImplementedRule();

    /**
     * {@code Subtype:  "subtype" | MemberName "." "subtype"	 }
     */
    Rule Subtype = new NotImplementedRule();

    /**
     * {@code SwitchCaseElse:  Switch ( Cases | "{" Cases "}" )	 }
     */
    Rule SwitchCaseElse = new NotImplementedRule();

    /**
     * {@code Switch:  "switch" "(" Expression ")"	 }
     */
    Rule Switch = new NotImplementedRule();

    /**
     * {@code Throw:  "throw" Expression?	 }
     */
    Rule Throw = new NotImplementedRule();

    /**
     * {@code TimeLiteral:   "'"  Digit{1,2} ":" Digit{2} ( ":" Digit{2} ( ":" Digit{3} )? )?  (" " "AM"|"PM")?  (" " Character{3,4})?  "'"	 }
     */
    Rule TimeLiteral = new NotImplementedRule();

    /**
     * {@code TryCatchFinally:  Try Catch* Finally?	 }
     */
    Rule TryCatchFinally = new NotImplementedRule();

    /**
     * {@code Try:  "try" ("(" Resource ")")? Block	 }
     */
    Rule Try = new NotImplementedRule();

    /**
     * {@code TypeArguments:  "<" (UnionType ",")* (UnionType | SequencedType) ">" }
     */
    Rule TypeArguments = new NotImplementedRule();

    /**
     * {@code TypeArgument:  UnionType | Dimension	 }
     */
    Rule TypeArgument = new NotImplementedRule();

    /**
     * {@code TypeConstraint:  "given" TypeName TypeParams? Params? TypeConstraintInheritance	 }
     */
    Rule TypeConstraint = new NotImplementedRule();

    /**
     * {@code TypeConstraintInheritance:  CaseTypes? Metatypes? SatisfiedTypes? AbstractedType?	 }
     */
    Rule TypeConstraintInheritance = new NotImplementedRule();

    /**
     * {@code TypeConstraints:  TypeConstraint+	 }
     */
    Rule TypeConstraints = new NotImplementedRule();

    /**
     * {@code TypeDeclaration:  Class | Object | Interface	 }
     */
    Rule TypeDeclaration = new NotImplementedRule();

    /**
     * {@code TypedQuotedLiteral:  TypeName QuotedLiteral	 }
     */
    Rule TypedQuotedLiteral = new NotImplementedRule();

    /**
     * {@code TypedVariable:  UnionType MemberName	 }
     */
    Rule TypedVariable = new NotImplementedRule();

    /**
     * {@code TypeMeta:  Type	 }
     */
    Rule TypeMeta = new NotImplementedRule();

    /**
     * {@code TypeNameWithArguments:  TypeName TypeArguments?	 }
     */
    Rule TypeNameWithArguments = new NotImplementedRule();

    /**
     * {@code TypeParams:  "<" (TypeParam ",")* (TypeParam | SequencedTypeParam) ">"	 }
     */
    Rule TypeParams = new NotImplementedRule();

    /**
     * {@code TypeParam:  Variance? TypeName	 }
     */
    Rule TypeParam = new NotImplementedRule();

    /**
     * {@code TypeSpecifier:  "=" Type	 }
     */
    Rule TypeSpecifier = new NotImplementedRule();

    /**
     * {@code Type:  TypeNameWithArguments ("." TypeNameWithArguments)*	 }
     */
    Rule Type = new NotImplementedRule();

    /**
     * {@code UnionType:  IntersectionType ("|" IntersectionType)* }
     */
    Rule UnionType = new NotImplementedRule();

    /**
     * {@code UppercaseChar:  "A".."Z" ;	 }
     */
    Rule UppercaseChar = new NotImplementedRule();

    /**
     * {@code ValueMeta:  MemberName TypeArguments?	 }
     */
    Rule ValueMeta = new NotImplementedRule();

    /**
     * {@code ValueReference:  (Receiver ".")? MemberName	 }
     */
    Rule ValueReference = new NotImplementedRule();

    /**
     * {@code Variable:  UnionType? MemberName	 }
     */
    Rule Variable = new NotImplementedRule();

    /**
     * {@code Variance:  "out" | "in"	 }
     */
    Rule Variance = new NotImplementedRule();

    /**
     * {@code While:  LoopCondition Block	 }
     */
    Rule While = new NotImplementedRule();

    /**
     * {@code Whitespace:  " " | Tab | Formfeed | Newline | Return	 }
     */
    Rule Whitespace = new NotImplementedRule();


    boolean parseRequired(PsiBuilder builder);

    boolean parseOptional(PsiBuilder builder);


}
