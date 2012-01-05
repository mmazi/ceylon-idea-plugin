package org.ceylon.idea.lang.parser;

import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.IFileElementType;
import com.intellij.psi.tree.ILightStubFileElementType;
import org.ceylon.idea.Ceylon;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

public final class CeylonAstNode extends IElementType {
    public static final IFileElementType FILE = new ILightStubFileElementType(Ceylon.LANGUAGE);

    public static final CeylonAstNode ABSTRACTED_TYPE = new CeylonAstNode("AbstractedType");
    public static final CeylonAstNode ADAPTED_TYPES = new CeylonAstNode("AdaptedTypes");
    public static final CeylonAstNode ADD_ASSIGN_OP = new CeylonAstNode("AddAssignOp");
    public static final CeylonAstNode ALIAS = new CeylonAstNode("Alias");
    public static final CeylonAstNode AND_ASSIGN_OP = new CeylonAstNode("AndAssignOp");
    public static final CeylonAstNode AND_OP = new CeylonAstNode("AndOp");
    public static final CeylonAstNode ANNOTATION = new CeylonAstNode("Annotation");
    public static final CeylonAstNode ANNOTATION_LIST = new CeylonAstNode("AnnotationList");
    public static final CeylonAstNode ANY_CLASS = new CeylonAstNode("AnyClass");
    public static final CeylonAstNode ANY_INTERFACE = new CeylonAstNode("AnyInterface");
    public static final CeylonAstNode ANY_METHOD = new CeylonAstNode("AnyMethod");
    public static final CeylonAstNode APPLY_OP = new CeylonAstNode("ApplyOp");
    public static final CeylonAstNode ASSIGN_OP = new CeylonAstNode("AssignOp");
    public static final CeylonAstNode ATTRIBUTE_ARGUMENT = new CeylonAstNode("AttributeArgument");
    public static final CeylonAstNode ATTRIBUTE_DECLARATION = new CeylonAstNode("AttributeDeclaration");
    public static final CeylonAstNode ATTRIBUTE_GETTER_DEFINITION = new CeylonAstNode("AttributeGetterDefinition");
    public static final CeylonAstNode ATTRIBUTE_SETTER_DEFINITION = new CeylonAstNode("AttributeSetterDefinition");
    public static final CeylonAstNode BASE_MEMBER_EXPRESSION = new CeylonAstNode("BaseMemberExpression");
    public static final CeylonAstNode BASE_TYPE_EXPRESSION = new CeylonAstNode("BaseTypeExpression");
    public static final CeylonAstNode BASE_TYPE = new CeylonAstNode("BaseType");
    public static final CeylonAstNode BLOCK = new CeylonAstNode("Block");
    public static final CeylonAstNode BOOLEAN_CONDITION = new CeylonAstNode("BooleanCondition");
    public static final CeylonAstNode BREAK = new CeylonAstNode("Break");
    public static final CeylonAstNode CASE_CLAUSE = new CeylonAstNode("CaseClause");
    public static final CeylonAstNode CASE_ITEM = new CeylonAstNode("CaseItem");
    public static final CeylonAstNode CASE_TYPES = new CeylonAstNode("CaseTypes");
    public static final CeylonAstNode CATCH_CLAUSE = new CeylonAstNode("CatchClause");
    public static final CeylonAstNode CATCH_VARIABLE = new CeylonAstNode("CatchVariable");
    public static final CeylonAstNode CHAR_LITERAL = new CeylonAstNode("CharLiteral");
    public static final CeylonAstNode CLASS_BODY = new CeylonAstNode("ClassBody");
    public static final CeylonAstNode CLASS_DECLARATION = new CeylonAstNode("ClassDeclaration");
    public static final CeylonAstNode CLASS_DEFINITION = new CeylonAstNode("ClassDefinition");
    public static final CeylonAstNode COMPARE_OP = new CeylonAstNode("CompareOp");
    public static final CeylonAstNode COMPILATION_UNIT = new CeylonAstNode("CompilationUnit");
    public static final CeylonAstNode COMPILER_ANNOTATION = new CeylonAstNode("CompilerAnnotation");
    public static final CeylonAstNode COMPLEMENT_ASSIGN_OP = new CeylonAstNode("ComplementAssignOp");
    public static final CeylonAstNode COMPLEMENT_OP = new CeylonAstNode("ComplementOp");
    public static final CeylonAstNode COMPREHENSION = new CeylonAstNode("Comprehension");
    public static final CeylonAstNode CONTINUE = new CeylonAstNode("Continue");
    public static final CeylonAstNode CONTROL_CLAUSE = new CeylonAstNode("ControlClause");
    public static final CeylonAstNode CONTROL_STATEMENT = new CeylonAstNode("ControlStatement");
    public static final CeylonAstNode DECREMENT_OP = new CeylonAstNode("DecrementOp");
    public static final CeylonAstNode DEFAULT_ARGUMENT = new CeylonAstNode("DefaultArgument");
    public static final CeylonAstNode DEFAULT_OP = new CeylonAstNode("DefaultOp");
    public static final CeylonAstNode DIFFERENCE_OP = new CeylonAstNode("DifferenceOp");
    public static final CeylonAstNode DIVIDE_ASSIGN_OP = new CeylonAstNode("DivideAssignOp");
    public static final CeylonAstNode ELEMENT = new CeylonAstNode("Element");
    public static final CeylonAstNode ELEMENT_RANGE = new CeylonAstNode("ElementRange");
    public static final CeylonAstNode ELLIPSIS = new CeylonAstNode("Ellipsis");
    public static final CeylonAstNode ELSE_CLAUSE = new CeylonAstNode("ElseClause");
    public static final CeylonAstNode ENTRY_OP = new CeylonAstNode("EntryOp");
    public static final CeylonAstNode EQUAL_OP = new CeylonAstNode("EqualOp");
    public static final CeylonAstNode ERASURE = new CeylonAstNode("Erasure");
    public static final CeylonAstNode EXISTS_CONDITION = new CeylonAstNode("ExistsCondition");
    public static final CeylonAstNode EXISTS = new CeylonAstNode("Exists");
    public static final CeylonAstNode EXPRESSION_COMPREHENSION_CLAUSE = new CeylonAstNode("ExpressionComprehensionClause");
    public static final CeylonAstNode EXPRESSION = new CeylonAstNode("Expression");
    public static final CeylonAstNode EXPRESSION_LIST = new CeylonAstNode("ExpressionList");
    public static final CeylonAstNode EXPRESSION_STATEMENT = new CeylonAstNode("ExpressionStatement");
    public static final CeylonAstNode EXTENDED_TYPE_EXPRESSION = new CeylonAstNode("ExtendedTypeExpression");
    public static final CeylonAstNode EXTENDED_TYPE = new CeylonAstNode("ExtendedType");
    public static final CeylonAstNode EXTENDS = new CeylonAstNode("Extends");
    public static final CeylonAstNode FINALLY_CLAUSE = new CeylonAstNode("FinallyClause");
    public static final CeylonAstNode FLIP_OP = new CeylonAstNode("FlipOp");
    public static final CeylonAstNode FLOAT_LITERAL = new CeylonAstNode("FloatLiteral");
    public static final CeylonAstNode FOR_CLAUSE = new CeylonAstNode("ForClause");
    public static final CeylonAstNode FOR_COMPREHENSION_CLAUSE = new CeylonAstNode("ForComprehensionClause");
    public static final CeylonAstNode FOR_ITERATOR = new CeylonAstNode("ForIterator");
    public static final CeylonAstNode FOR_STATEMENT = new CeylonAstNode("ForStatement");
    public static final CeylonAstNode FUNCTIONAL_PARAMETER_DECLARATION = new CeylonAstNode("FunctionalParameterDeclaration");
    public static final CeylonAstNode FUNCTION_ARGUMENT = new CeylonAstNode("FunctionArgument");
    public static final CeylonAstNode FUNCTION_MODIFIER = new CeylonAstNode("FunctionModifier");
    public static final CeylonAstNode IDENTICAL_OP = new CeylonAstNode("IdenticalOp");
    public static final CeylonAstNode IDENTIFIER = new CeylonAstNode("Identifier");
    public static final CeylonAstNode IF_CLAUSE = new CeylonAstNode("IfClause");
    public static final CeylonAstNode IF_COMPREHENSION_CLAUSE = new CeylonAstNode("IfComprehensionClause");
    public static final CeylonAstNode IF_STATEMENT = new CeylonAstNode("IfStatement");
    public static final CeylonAstNode IMPLICIT = new CeylonAstNode("Implicit");
    public static final CeylonAstNode IMPORT = new CeylonAstNode("Import");
    public static final CeylonAstNode IMPORT_LIST = new CeylonAstNode("ImportList");
    public static final CeylonAstNode IMPORT_MEMBER = new CeylonAstNode("ImportMember");
    public static final CeylonAstNode IMPORT_MEMBER_OR_TYPE = new CeylonAstNode("ImportMemberOrType");
    public static final CeylonAstNode IMPORT_MEMBER_OR_TYPE_LIST = new CeylonAstNode("ImportMemberOrTypeList");
    public static final CeylonAstNode IMPORT_PATH = new CeylonAstNode("ImportPath");
    public static final CeylonAstNode IMPORT_TYPE = new CeylonAstNode("ImportType");
    public static final CeylonAstNode IMPORT_WILDCARD = new CeylonAstNode("ImportWildcard");
    public static final CeylonAstNode INCREMENT_OP = new CeylonAstNode("IncrementOp");
    public static final CeylonAstNode INDEX_EXPRESSION = new CeylonAstNode("IndexExpression");
    public static final CeylonAstNode INDEX_OP = new CeylonAstNode("IndexOp");
    public static final CeylonAstNode INFERRED_TYPE_ARGUMENTS = new CeylonAstNode("InferredTypeArguments");
    public static final CeylonAstNode INITIALIZER_EXPRESSION = new CeylonAstNode("InitializerExpression");
    public static final CeylonAstNode IN_OP = new CeylonAstNode("InOp");
    public static final CeylonAstNode INTERFACE_BODY = new CeylonAstNode("InterfaceBody");
    public static final CeylonAstNode INTERFACE_DECLARATION = new CeylonAstNode("InterfaceDeclaration");
    public static final CeylonAstNode INTERFACE_DEFINITION = new CeylonAstNode("InterfaceDefinition");
    public static final CeylonAstNode INTERSECT_ASSIGN_OP = new CeylonAstNode("IntersectAssignOp");
    public static final CeylonAstNode INTERSECTION_OP = new CeylonAstNode("IntersectionOp");
    public static final CeylonAstNode INTERSECTION_TYPE = new CeylonAstNode("IntersectionType");
    public static final CeylonAstNode INVOCATION_EXPRESSION = new CeylonAstNode("InvocationExpression");
    public static final CeylonAstNode IS_CASE = new CeylonAstNode("IsCase");
    public static final CeylonAstNode IS_CONDITION = new CeylonAstNode("IsCondition");
    public static final CeylonAstNode IS_OP = new CeylonAstNode("IsOp");
    public static final CeylonAstNode KEY_VALUE_ITERATOR = new CeylonAstNode("KeyValueIterator");
    public static final CeylonAstNode LAMBDA = new CeylonAstNode("Lambda");
    public static final CeylonAstNode LARGE_AS_OP = new CeylonAstNode("LargeAsOp");
    public static final CeylonAstNode LARGER_OP = new CeylonAstNode("LargerOp");
    public static final CeylonAstNode MATCH_CASE = new CeylonAstNode("MatchCase");
    public static final CeylonAstNode MEMBER_OP = new CeylonAstNode("MemberOp");
    public static final CeylonAstNode METHOD_ARGUMENT = new CeylonAstNode("MethodArgument");
    public static final CeylonAstNode METHOD_DECLARATION = new CeylonAstNode("MethodDeclaration");
    public static final CeylonAstNode METHOD_DEFINITION = new CeylonAstNode("MethodDefinition");
    public static final CeylonAstNode MULTIPLY_ASSIGN_OP = new CeylonAstNode("MultiplyAssignOp");
    public static final CeylonAstNode NAMED_ARGUMENT_LIST = new CeylonAstNode("NamedArgumentList");
    public static final CeylonAstNode NATURAL_LITERAL = new CeylonAstNode("NaturalLiteral");
    public static final CeylonAstNode NEGATIVE_OP = new CeylonAstNode("NegativeOp");
    public static final CeylonAstNode NONEMPTY_CONDITION = new CeylonAstNode("NonemptyCondition");
    public static final CeylonAstNode NONEMPTY = new CeylonAstNode("Nonempty");
    public static final CeylonAstNode NOT_EQUAL_OP = new CeylonAstNode("NotEqualOp");
    public static final CeylonAstNode NOT_OP = new CeylonAstNode("NotOp");
    public static final CeylonAstNode OBJECT_ARGUMENT = new CeylonAstNode("ObjectArgument");
    public static final CeylonAstNode OBJECT_DEFINITION = new CeylonAstNode("ObjectDefinition");
    public static final CeylonAstNode OR_ASSIGN_OP = new CeylonAstNode("OrAssignOp");
    public static final CeylonAstNode OR_OP = new CeylonAstNode("OrOp");
    public static final CeylonAstNode OUTER = new CeylonAstNode("Outer");
    public static final CeylonAstNode PARAMETER_LIST = new CeylonAstNode("ParameterList");
    public static final CeylonAstNode POSITIONAL_ARGUMENT = new CeylonAstNode("PositionalArgument");
    public static final CeylonAstNode POSITIONAL_ARGUMENT_LIST = new CeylonAstNode("PositionalArgumentList");
    public static final CeylonAstNode POSITIVE_OP = new CeylonAstNode("PositiveOp");
    public static final CeylonAstNode POSTFIX_DECREMENT_OP = new CeylonAstNode("PostfixDecrementOp");
    public static final CeylonAstNode POSTFIX_INCREMENT_OP = new CeylonAstNode("PostfixIncrementOp");
    public static final CeylonAstNode POWER_OP = new CeylonAstNode("PowerOp");
    public static final CeylonAstNode PRODUCT_OP = new CeylonAstNode("ProductOp");
    public static final CeylonAstNode QUALIFIED_MEMBER_EXPRESSION = new CeylonAstNode("QualifiedMemberExpression");
    public static final CeylonAstNode QUALIFIED_TYPE_EXPRESSION = new CeylonAstNode("QualifiedTypeExpression");
    public static final CeylonAstNode QUALIFIED_TYPE = new CeylonAstNode("QualifiedType");
    public static final CeylonAstNode QUOTED_LITERAL = new CeylonAstNode("QuotedLiteral");
    public static final CeylonAstNode QUOTIENT_OP = new CeylonAstNode("QuotientOp");
    public static final CeylonAstNode RANGE_OP = new CeylonAstNode("RangeOp");
    public static final CeylonAstNode REMAINDER_ASSIGN_OP = new CeylonAstNode("RemainderAssignOp");
    public static final CeylonAstNode REMAINDER_OP = new CeylonAstNode("RemainderOp");
    public static final CeylonAstNode RESOURCE = new CeylonAstNode("Resource");
    public static final CeylonAstNode RETURN = new CeylonAstNode("Return");
    public static final CeylonAstNode SAFE_INDEX_OP = new CeylonAstNode("SafeIndexOp");
    public static final CeylonAstNode SAFE_MEMBER_OP = new CeylonAstNode("SafeMemberOp");
    public static final CeylonAstNode SATISFIED_TYPES = new CeylonAstNode("SatisfiedTypes");
    public static final CeylonAstNode SATISFIES_CASE = new CeylonAstNode("SatisfiesCase");
    public static final CeylonAstNode SATISFIES_CONDITION = new CeylonAstNode("SatisfiesCondition");
    public static final CeylonAstNode SATISFIES = new CeylonAstNode("Satisfies");
    public static final CeylonAstNode SEQUENCED_ARGUMENT = new CeylonAstNode("SequencedArgument");
    public static final CeylonAstNode SEQUENCED_TYPE = new CeylonAstNode("SequencedType");
    public static final CeylonAstNode SEQUENCED_TYPE_PARAMETER_DECLARATION = new CeylonAstNode("SequencedTypeParameterDeclaration");
    public static final CeylonAstNode SEQUENCE_ENUMERATION = new CeylonAstNode("SequenceEnumeration");
    public static final CeylonAstNode SMALL_AS_OP = new CeylonAstNode("SmallAsOp");
    public static final CeylonAstNode SMALLER_OP = new CeylonAstNode("SmallerOp");
    public static final CeylonAstNode SPECIFIED_ARGUMENT = new CeylonAstNode("SpecifiedArgument");
    public static final CeylonAstNode SPECIFIER_EXPRESSION = new CeylonAstNode("SpecifierExpression");
    public static final CeylonAstNode SPECIFIER_STATEMENT = new CeylonAstNode("SpecifierStatement");
    public static final CeylonAstNode SPREAD_OP = new CeylonAstNode("SpreadOp");
    public static final CeylonAstNode STRING_LITERAL = new CeylonAstNode("StringLiteral");
    public static final CeylonAstNode STRING_TEMPLATE = new CeylonAstNode("StringTemplate");
    public static final CeylonAstNode SUBTRACT_ASSIGN_OP = new CeylonAstNode("SubtractAssignOp");
    public static final CeylonAstNode SUM_OP = new CeylonAstNode("SumOp");
    public static final CeylonAstNode SUPER = new CeylonAstNode("Super");
    public static final CeylonAstNode SUPER_TYPE = new CeylonAstNode("SuperType");
    public static final CeylonAstNode SWITCH_CASE_LIST = new CeylonAstNode("SwitchCaseList");
    public static final CeylonAstNode SWITCH_CLAUSE = new CeylonAstNode("SwitchClause");
    public static final CeylonAstNode SWITCH_STATEMENT = new CeylonAstNode("SwitchStatement");
    public static final CeylonAstNode SYNTHETIC_VARIABLE = new CeylonAstNode("SyntheticVariable");
    public static final CeylonAstNode THEN_OP = new CeylonAstNode("ThenOp");
    public static final CeylonAstNode THIS = new CeylonAstNode("This");
    public static final CeylonAstNode THROW = new CeylonAstNode("Throw");
    public static final CeylonAstNode TRY_CATCH_STATEMENT = new CeylonAstNode("TryCatchStatement");
    public static final CeylonAstNode TRY_CLAUSE = new CeylonAstNode("TryClause");
    public static final CeylonAstNode TYPE_ARGUMENT_LIST = new CeylonAstNode("TypeArgumentList");
    public static final CeylonAstNode TYPE_ARGUMENTS = new CeylonAstNode("TypeArguments");
    public static final CeylonAstNode TYPE_CONSTRAINT = new CeylonAstNode("TypeConstraint");
    public static final CeylonAstNode TYPE_CONSTRAINT_LIST = new CeylonAstNode("TypeConstraintList");
    public static final CeylonAstNode TYPE_PARAMETER_DECLARATION = new CeylonAstNode("TypeParameterDeclaration");
    public static final CeylonAstNode TYPE_PARAMETER_LIST = new CeylonAstNode("TypeParameterList");
    public static final CeylonAstNode TYPE_SPECIFIER = new CeylonAstNode("TypeSpecifier");
    public static final CeylonAstNode TYPE_VARIANCE = new CeylonAstNode("TypeVariance");
    public static final CeylonAstNode UNION_ASSIGN_OP = new CeylonAstNode("UnionAssignOp");
    public static final CeylonAstNode UNION_OP = new CeylonAstNode("UnionOp");
    public static final CeylonAstNode UNION_TYPE = new CeylonAstNode("UnionType");
    public static final CeylonAstNode VALUE_ITERATOR = new CeylonAstNode("ValueIterator");
    public static final CeylonAstNode VALUE_MODIFIER = new CeylonAstNode("ValueModifier");
    public static final CeylonAstNode VALUE_PARAMETER_DECLARATION = new CeylonAstNode("ValueParameterDeclaration");
    public static final CeylonAstNode VARIABLE = new CeylonAstNode("Variable");
    public static final CeylonAstNode VOID_MODIFIER = new CeylonAstNode("VoidModifier");
    public static final CeylonAstNode WHILE_CLAUSE = new CeylonAstNode("WhileClause");
    public static final CeylonAstNode WHILE_STATEMENT = new CeylonAstNode("WhileStatement");
    public static final CeylonAstNode XOR_ASSIGN_OP = new CeylonAstNode("XorAssignOp");
    public static final CeylonAstNode XOR_OP = new CeylonAstNode("XorOp");

    private CeylonAstNode(@NotNull @NonNls String debugName) {
        super(debugName, Ceylon.LANGUAGE);
    }

}
