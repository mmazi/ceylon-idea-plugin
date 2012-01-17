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
     * {@code UnionType:  IntersectionType ("|" IntersectionType)* }
     *
     * @see #deferredInit()
     */
    public static final ComplexRule UnionType = rule("UnionType");

    /**
     * {@code IfElse:  If Else?	 }
     *
     * @see #deferredInit()
     */
    public static final ComplexRule IfElse = rule("IfElse");

    /**
     * {@code TypeName:  UIdentifier }
     */
    public static final Rule TypeName = rule("TypeName").one(UIDENTIFIER);

    /**
     * {@code MemberName:  LIdentifier	 }
     */
    public static final Rule MemberName = rule("MemberName").one(LIDENTIFIER);

    /**
     * {@code SequencedType:  TypeName "..."	 }
     */
    public static final Rule SequencedType = rule("SequencedType").sequence(TypeName, ELLIPSIS);

    /**
     * {@code TypeArguments:  "<" (UnionType ",")* (UnionType | SequencedType) ">" }
     */
    public static final Rule TypeArguments = rule("TypeArguments").one(SMALLER_OP).zeroOrMore(UnionType, COMMA).any(SequencedType, UnionType).one(LARGER_OP);

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
     * {@code Sequence:  Expression ("," Expression)* | Expression "..."	 }
     * TODO: differs from spec
     */
    public static final Rule Sequence = rule("Sequence").one(Expression).zeroOrMore(COMMA, Expression).zeroOrOne(ELLIPSIS);

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
     * {@code Literal:  IntegerLiteral | FloatLiteral | CharacterLiteral | StringLiteral | QuotedLiteral	 }
     */
    public static final Rule Literal = any(IntegerLiteral, FloatLiteral, CHAR_LITERAL, STRING_LITERAL, QUOTED_LITERAL);

    /**
     * {@code StringTemplate:  StringLiteral (Expression StringLiteral)+	 }
     */
    public static final Rule StringTemplate = rule("StringTemplate").one(STRING_LITERAL).oneOrMore(Expression, STRING_LITERAL);

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
     * {@code Receiver:  Primary	 }
     */
    public static final Rule Receiver = new DummyRule("Receiver");

    /**
     * {@code MethodReference:  (Receiver ".")? MemberName TypeArguments?	 }
     */
    public static final Rule MethodReference = rule("MethodReference").zeroOrOne(Receiver, MEMBER_OP).one(MemberName).zeroOrOne(TypeArguments);

    /**
     * {@code InitializerReference:  (Receiver ".")? TypeName TypeArguments?	 }
     */
    public static final Rule InitializerReference = rule("InitializerReference").zeroOrOne(Receiver, MEMBER_OP).one(TypeName).zeroOrOne(TypeArguments);

    /**
     * {@code CallableReference:  MethodReference | InitializerReference	 }
     */
    public static final Rule CallableReference = any(MethodReference, InitializerReference);

    /**
     * {@code ValueReference:  (Receiver ".")? MemberName	 }
     */
    public static final Rule ValueReference = rule("ValueReference").zeroOrOne(Receiver, MEMBER_OP).one(MemberName);

    /**
     * {@code MemberReference:  CallableReference | ValueReference	 }
     */
    public static final Rule MemberReference = rule("MemberReference").any(CallableReference, ValueReference);

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
    public static final Rule DefaultParam = rule("DefaultParam").sequence(Param, Specifier);

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
     * {@code Variance:  "out" | "in"	 }
     */
    public static final Rule Variance = rule("Variance").any(OUT, IN_OP);

    /**
     * {@code TypeParam:  Variance? TypeName	 }
     */
    public static final Rule TypeParam = rule("TypeParam").zeroOrOne(Variance).one(TypeName);

    /**
     * {@code SequencedTypeParam:  TypeName "..."	 }
     */
    public static final Rule SequencedTypeParam = rule("SequencedTypeParam").sequence(TypeName, ELLIPSIS);

    /**
     * {@code TypeParams:  "<" (TypeParam ",")* (TypeParam | SequencedTypeParam) ">"	 }
     */
    public static final Rule TypeParams = rule("TypeParams").one(SMALLER_OP).zeroOrMore(TypeParam, COMMA).any(SequencedTypeParam, TypeParam).one(LARGER_OP);

    /**
     * {@code CaseType:  MemberName | Type	 }
     */
    public static final Rule CaseType = rule("CaseType").any(MemberName, Type);

    /**
     * {@code CaseTypes:  "of" CaseType ("|" CaseType)*	 }
     */
    public static final Rule CaseTypes = rule("CaseTypes").sequence(CASE_TYPES, CaseType).zeroOrMore(UNION_OP, CaseType);

    /**
     * {@code "Metatypes:  "is" Type ("&" Type)*	 }
     */
    public static final Rule Metatypes = rule("Metatypes").sequence(IS_OP, Type).zeroOrMore(INTERSECTION_OP, Type);

    /**
     * {@code SatisfiedTypes:  "satisfies" Type ("&" Type)*	 }
     */
    public static final Rule SatisfiedTypes = rule("SatisfiedTypes").sequence(SATISFIES, Type).zeroOrMore(INTERSECTION_OP, Type);

    /**
     * {@code AbstractedType:  "abstracts" Type	 }
     */
    public static final Rule AbstractedType = rule("AbstractedType").sequence(ABSTRACTED_TYPE, Type);

    /**
     * {@code TypeConstraintInheritance:  CaseTypes? Metatypes? SatisfiedTypes? AbstractedType?	 }
     */
    public static final Rule TypeConstraintInheritance = rule("TypeConstraintInheritance").zeroOrOne(CaseTypes).zeroOrOne(Metatypes).zeroOrOne(SatisfiedTypes).zeroOrOne(AbstractedType);

    /**
     * {@code TypeConstraint:  "given" TypeName TypeParams? Params? TypeConstraintInheritance	 }
     */
    public static final Rule TypeConstraint = rule("TypeConstraint").sequence(TYPE_CONSTRAINT, TypeName).zeroOrOne(TypeParams).zeroOrOne(Params).one(TypeConstraintInheritance);

    /**
     * {@code TypeConstraints:  TypeConstraint+	 }
     */
    public static final Rule TypeConstraints = rule("TypeConstraints").oneOrMore(TypeConstraint);

    /**
     * {@code MethodHeader:  (UnionType | "function" | "void") MemberName TypeParams? Params+ Metatypes? TypeConstraints?	 }
     */
    public static final Rule MethodHeader = rule("MethodHeader").any(UnionType, FUNCTION_MODIFIER, VOID_MODIFIER).one(MemberName).zeroOrOne(TypeParams).oneOrMore(Params).zeroOrMore(Metatypes).zeroOrMore(TypeConstraints);

    /**
     * {@code AttributeHeader:  (UnionType | "value") MemberName	 }
     */
    public static final Rule AttributeHeader = rule("AttributeHeader").any(UnionType, VALUE_MODIFIER).one(MemberName);

    /**
     * {@code Initializer:  ":=" Expression	 }
     */
    public static final Rule Initializer = rule("Initializer").sequence(ASSIGN_OP, Expression);

    /**
     * {@code SimpleAttribute:  AttributeHeader ( (Specifier | Initializer)? ";" | NamedArguments )	 }
     */
    public static final Rule SimpleAttribute = rule("SimpleAttribute").one(AttributeHeader).any(sequence(zeroOrAny(Specifier, Initializer), SEMICOLON), NamedArguments);

    /**
     * {@code AttributeGetter:  AttributeHeader Block	 }
     */
    public static final Rule AttributeGetter = rule("AttributeGetter").sequence(AttributeHeader, Block);

    /**
     * {@code AttributeSetter:  "assign" MemberName Block }
     */
    public static final Rule AttributeSetter = rule("AttributeSetter").sequence(ASSIGN, MemberName, Block);

    /**
     * {@code AdaptedTypes:  "adapts" Type ("&" Type)*	 }
     */
    public static final Rule AdaptedTypes = rule("AdaptedTypes").sequence(ADAPTED_TYPES, Type).zeroOrMore(INTERSECTION_OP, Type);

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
    public static final Rule BooleanCondition = rule("BooleanCondition").one(Expression);

    /**
     * {@code Break:  "break" }
     */
    public static final Rule Break = new NotImplementedRule("Break");

    /**
     * {@code CallableVariable:  (UnionType | "void")? MemberName Params+	 }
     */
    public static final Rule CallableVariable = rule("CallableVariable").zeroOrAny(UnionType, VOID_MODIFIER).one(MemberName).oneOrMore(Params);

    /**
     * {@code Class:  Annotation* ClassHeader (ClassBody | TypeSpecifier ";")	 }
     */
    public static final Rule Class = new DummyRule("Class");

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
     * {@code ConditionalTypes:  SatisfiedTypes Conditions	 }
     */
    public static final Rule ConditionalTypes = new NotImplementedRule("ConditionalTypes");

    /**
     * {@code Conditions:  "if" "(" Condition ("&&" Condition)* ")"	 }
     */
    public static final Rule Conditions = new NotImplementedRule("Conditions");

    /**
     * {@code Continue:  "continue"	 }
     */
    public static final Rule Continue = new NotImplementedRule("Continue");

    /**
     * {@code DateLiteral:   "'"  Digit{1,2} "/" Digit{1,2} "/" Digit{4}  "'"	 }
     */
    public static final Rule DateLiteral = new NotImplementedRule("DateLiteral");

    /**
     * {@code Digit:  "0".."9"	 }
     */
    public static final Rule Digit = new NotImplementedRule("Digit");

    /**
     * {@code AnnotationName:  LIdentifier }
     */
    public static final Rule AnnotationName = rule("AnnotationName").one(LIDENTIFIER);

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
     * {@code Variable:  UnionType? MemberName	 }
     */
    public static final Rule Variable = rule("Variable").zeroOrOne(UnionType).one(MemberName);

    /**
     * {@code EntryVariablePair:  Variable "->" Variable	 }
     */
    public static final Rule EntryVariablePair = rule("EntryVariablePair").sequence(Variable, ENTRY_OP, Variable);

    /**
     * {@code ExistsOrNonemptyCondition:  ("exists" | "nonempty") (Variable Specifier | MemberName)	 }
     */
    public static final Rule ExistsOrNonemptyCondition = rule("ExistsOrNonemptyCondition").any(EXISTS, NONEMPTY).any(sequence(Variable, Specifier), MemberName);

    /**
     * {@code Exponent:  ("E"|"e") ("+"|"-")? Digits	 }
     */
    public static final Rule Exponent = new NotImplementedRule("Exponent");

    /**
     * {@code ExtendedType:  "extends" ("super" ".")? Type PositionalArguments	 }
     */
    public static final Rule ExtendedType = new NotImplementedRule("ExtendedType");

    /**
     * {@code Fail:  "else" Block	 }
     */
    public static final Rule Fail = rule("Fail").sequence(ELSE_CLAUSE, Block);

    /**
     * {@code Finally:  "finally" Block	 }
     */
    public static final Rule Finally = rule("Finally").sequence(FINALLY_CLAUSE, Block);

    /**
     * {@code IteratorVariable:  Variable | CallableVariable | EntryVariablePair	 }
     */
    public static final Rule IteratorVariable = rule("IteratorVariable").any(CallableVariable, EntryVariablePair, Variable);

    /**
     * {@code ForIterator:  IteratorVariable "in" Expression	 }
     */
    public static final Rule ForIterator = rule("ForIterator").sequence(IteratorVariable, IN_OP, Expression);

    /**
     * {@code For:  "for" "(" ForIterator ")" Block	 }
     */
    public static final Rule For = rule("For").sequence(FOR_CLAUSE, LPAREN, ForIterator, RPAREN, Block);

    /**
     * {@code ForFail:  For Fail?	 }
     */
    public static final Rule ForFail = rule("ForFail").one(For).zeroOrOne(Fail);

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
     * {@code TypedVariable:  UnionType MemberName	 }
     */
    public static final Rule TypedVariable = rule("TypedVariable").sequence(UnionType, MemberName);

    /**
     * {@code IsCondition:  "is" (TypedVariable Specifier | UnionType MemberName)	 }
     */
    public static final Rule IsCondition = rule("IsCondition").one(IS_OP).any(sequence(TypedVariable, Specifier), sequence(UnionType, MemberName));

    /**
     * {@code SatisfiesCondition:  "satisfies" Type Type	 }
     */
    public static final Rule SatisfiesCondition = rule("SatisfiesCondition").sequence(SATISFIES, Type, Type);

    /**
     * {@code Condition:  BooleanCondition | IsCondition | ExistsOrNonemptyCondition | SatisfiesCondition	 }
     */
    public static final Rule Condition = any(BooleanCondition, IsCondition, ExistsOrNonemptyCondition, SatisfiesCondition);

    /**
     * {@code If:  "if" "(" Condition ")" Block	 }
     */
    public static final Rule If = rule("If").sequence(IF_CLAUSE, LPAREN, Condition, RPAREN, Block);

    /**
     * {@code Else:  "else" (Block | IfElse)	 }
     */
    public static final Rule Else = rule("Else").one(ELSE_CLAUSE).any(Block, IfElse);

    /**
     * {@code IncrementOrDecrement:  "--" | "++" ; }
     */
    public static final Rule IncrementOrDecrement = new NotImplementedRule("IncrementOrDecrement");

    /**
     * {@code Interface:  Annotation* InterfaceHeader (InterfaceBody | TypeSpecifier ";")	 }
     */
    public static final Rule Interface = new DummyRule("Interface");

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
     * {@code LoopCondition:  "while" "(" Condition ")"	 }
     */
    public static final Rule LoopCondition = rule("LoopCondition").sequence(WHILE_CLAUSE, LPAREN, Condition, RPAREN);

    /**
     * {@code Magnitude:  "k" | "M" | "G" | "T" | "P"	 }
     */
    public static final Rule Magnitude = new NotImplementedRule("Magnitude");

    /**
     * {@code MethodMeta:  Type "." MemberName TypeArguments?	 }
     */
    public static final Rule MethodMeta = new NotImplementedRule("MethodMeta");

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
     * {@code Resource:  MemberName | InitializerReference Arguments | Variable Specifier	 }
     */
    public static final Rule Resource = rule("Resource").any(sequence(InitializerReference, Arguments), sequence(Variable, Specifier), MemberName);

    /**
     * {@code Retry:  "retry"	 }
     */
    public static final Rule Retry = new NotImplementedRule("Retry");

    /**
     * {@code Return:  "return" Expression?	 }
     */
    public static final Rule Return = new NotImplementedRule("Return");

    /**
     * {@code SequenceInstantiation:  "{" Sequence? "}" ;	 }
     */
    public static final Rule SequenceInstantiation = new NotImplementedRule("SequenceInstantiation");

    /**
     * {@code Subtype:  "subtype" | MemberName "." "subtype"	 }
     */
    public static final Rule Subtype = new NotImplementedRule("Subtype");

    /**
     * {@code Switch:  "switch" "(" Expression ")"	 }
     */
    public static final Rule Switch = rule("Switch").sequence(SWITCH_CLAUSE, LPAREN, Expression, RPAREN);

    /**
     * {@code Case:  Expression ("," Expression)* | "is" UnionType | "satisfies" Type	 }
     */
    public static final Rule Case = rule("Case").any(sequence(IS_OP, UnionType), sequence(SATISFIES, Type), sequence(Expression, zeroOrMore(COMMA, Expression)));

    /**
     * {@code CaseItem:  "case" "(" Case ")" Block	 }
     */
    public static final Rule CaseItem = rule("CaseItem").sequence(CASE_CLAUSE, LPAREN, Case, RPAREN, Block);

    /**
     * {@code DefaultCaseItem:  "else" Block	 }
     */
    public static final Rule DefaultCaseItem = rule("DefaultCaseItem").sequence(ELSE_CLAUSE, Block);

    /**
     * {@code Cases:  CaseItem+ DefaultCaseItem?	 }
     */
    public static final Rule Cases = rule("Cases").oneOrMore(CaseItem).zeroOrOne(DefaultCaseItem);

    /**
     * {@code SwitchCaseElse:  Switch ( Cases | "{" Cases "}" )	 }
     */
    public static final Rule SwitchCaseElse = rule("SwitchCaseElse").one(Switch).any(Cases, sequence(LBRACE, Cases, RBRACE));

    /**
     * {@code Throw:  "throw" Expression?	 }
     */
    public static final Rule Throw = new NotImplementedRule("Throw");

    /**
     * {@code TimeLiteral:   "'"  Digit{1,2} ":" Digit{2} ( ":" Digit{2} ( ":" Digit{3} )? )?  (" " "AM"|"PM")?  (" " Character{3,4})?  "'"	 }
     */
    public static final Rule TimeLiteral = new NotImplementedRule("TimeLiteral");

    /**
     * {@code Try:  "try" ("(" Resource ")")? Block	 }
     */
    public static final Rule Try = rule("Try").one(TRY_CLAUSE).zeroOrOne(LPAREN, Resource, RPAREN).one(Block);

    /**
     * {@code Catch:  "catch" "(" Variable ")" Block	 }
     */
    public static final Rule Catch = rule("Catch").sequence(CATCH_CLAUSE, LPAREN, Variable, RPAREN, Block);

    /**
     * {@code TryCatchFinally:  Try Catch* Finally?	 }
     */
    public static final Rule TryCatchFinally = rule("TryCatchFinally").one(Try).zeroOrMore(Catch).zeroOrOne(Finally);

    /**
     * {@code TypeArgument:  UnionType | Dimension	 }
     */
    public static final Rule TypeArgument = new NotImplementedRule("TypeArgument");

    /**
     * {@code TypedQuotedLiteral:  TypeName QuotedLiteral	 }
     */
    public static final Rule TypedQuotedLiteral = new NotImplementedRule("TypedQuotedLiteral");

    /**
     * {@code TypeMeta:  Type	 }
     */
    public static final Rule TypeMeta = new NotImplementedRule("TypeMeta");

    /**
     * {@code TypeSpecifier:  "=" Type	 }
     */
    public static final Rule TypeSpecifier = new NotImplementedRule("TypeSpecifier");

    /**
     * {@code ValueMeta:  MemberName TypeArguments?	 }
     */
    public static final Rule ValueMeta = new NotImplementedRule("ValueMeta");

    /**
     * {@code While:  LoopCondition Block	 }
     */
    public static final Rule While = rule("While").sequence(LoopCondition, Block);

    /**
     * {@code ExpressionStatement:  ( Assignment | IncrementOrDecrement | Invocation ) ";"	 }
     */
    public static final Rule ExpressionStatement = new DummyRule("ExpressionStatement");

    /**
     * {@code Specification:  MemberName Specifier ";"	 }
     */
    public static final Rule Specification = rule("Specification").sequence(MemberName, Specifier, SEMICOLON);

    /**
     * {@code DirectiveStatement:  Directive ";"	 }
     */
    public static final Rule DirectiveStatement = new DummyRule("DirectiveStatement");

    /**
     * {@code ControlStructure:  IfElse | SwitchCaseElse | While | ForFail | TryCatchFinally	 }
     */
    public static final Rule ControlStructure = any(IfElse, SwitchCaseElse, While, ForFail, TryCatchFinally);

    /**
     * {@code Statement:  ExpressionStatement | Specification | DirectiveStatement | ControlStructure	 }
     */
    public static final Rule Statement = rule("Statement").any(ExpressionStatement, Specification, DirectiveStatement, ControlStructure);

    /**
     * {@code CompilerAnnotation : "@" AnnotationName ("[" StringLiteral "]")? }
     */
    public static final Rule CompilerAnnotation = rule("CompilerAnnotation").sequence(COMPILER_ANNOTATION, AnnotationName).zeroOrOne(INDEX_OP, STRING_LITERAL, RBRACKET);


    /**
     * {@code CompilerAnnotations: CompilerAnnotation* }
     */
    public static final Rule CompilerAnnotations = zeroOrMore(CompilerAnnotation);

    /**
     * {@code Import:  "import" FullPackageName "{" ImportElements? "}" }
     */
    public static final Rule Import = rule("Import").sequence(IMPORT, FullPackageName).one(LBRACE).zeroOrOne(ImportElements).one(RBRACE);

    /**
     * {@code Method:  Annotation* MethodHeader (Block | NamedArguments | Specifier? ";")	 }
     */
    public static final Rule Method = rule("Method").zeroOrOne(Annotation).one(MethodHeader).any(Block, NamedArguments, sequence(zeroOrOne(Specifier), SEMICOLON));


    /**
     * {@code Attribute:  Annotation* (SimpleAttribute | AttributeGetter | AttributeSetter)	 }
     */
    public static final Rule Attribute = rule("Attribute").zeroOrMore(Annotation).any(SimpleAttribute, AttributeGetter, AttributeSetter);

    /**
     * {@code TypeDeclaration:  Class | Object | Interface	 }
     */
    public static final Rule TypeDeclaration = rule("TypeDeclaration").any(Class, Object, Interface);

    /**
     * {@code Declaration:  Method | Attribute | TypeDeclaration }
     */
    public static final Rule Declaration = rule("Declaration").any(Method, Attribute, TypeDeclaration);

    /**
     * {@code CompilationUnit : (CompilerAnnotation+ ";")? import* (CompilerAnnotations Declaration)* EOF }
     */
    public static final Rule CompilationUnit = rule("CompilationUnit").zeroOrOne(CompilerAnnotation, CompilerAnnotations, SEMICOLON).zeroOrMore(Import).zeroOrMore(CompilerAnnotations, Declaration);

    static {
        deferredInit();
    }

    private static void deferredInit() {
        Block.one(LBRACE).zeroOrAny(Declaration, Statement).one(RBRACE);
        Expression.any(Primary, OperatorExpression);
        NamedArguments.one(LBRACE).zeroOrMore(NamedArgument).zeroOrOne(Sequence).one(RBRACE);
        Param.zeroOrMore(Annotation).any(SimpleParam, CallableParam, EntryParamPair);
        Annotation.one(MemberName).zeroOrAny(Arguments, oneOrMore(Literal));
        UnionType.one(IntersectionType).zeroOrMore(UNION_OP, IntersectionType);
        IfElse.one(If).zeroOrOne(Else);
    }

}
