package org.ceylon.idea.lang.lexer;

import com.intellij.lang.PsiBuilder;
import com.intellij.psi.tree.IElementType;
import org.ceylon.idea.lang.parser.ParserUtils;
import org.ceylon.idea.lang.parser.rule.Rule;

public enum CeylonToken implements Rule {
    EOF(-1),
    BAD_CHARACTER(0),
    ABSTRACTED_TYPE(4),
    ADAPTED_TYPES(5),
    ADD_ASSIGN_OP(6),
    AND_ASSIGN_OP(7),
    AND_OP(8),
    APPLY_OP(9),
    ARRAY(10),
    ASSIGN(11),
    ASSIGN_OP(12),
    BREAK(13),
    CASE_CLAUSE(14),
    CASE_TYPES(15),
    CATCH_CLAUSE(16),
    CHAR_LITERAL(17),
    CLASS_DEFINITION(18),
    COMMA(19),
    COMPARE_OP(20),
    COMPILER_ANNOTATION(21),
    COMPLEMENT_ASSIGN_OP(22),
    COMPLEMENT_OP(23),
    CONTINUE(24),
    DECREMENT_OP(25),
    DEFAULT_OP(26),
    DIFFERENCE_OP(27),
    DIVIDE_ASSIGN_OP(28),
    Digit(29),
    Digits(30),
    ELLIPSIS(31),
    ELSE_CLAUSE(32),
    ENTRY_OP(33),
    EQUAL_OP(34),
    EXISTS(35),
    EXTENDS(36),
    EscapeSequence(37),
    Exponent(38),
    FINALLY_CLAUSE(39),
    FLOAT_LITERAL(40),
    FOR_CLAUSE(41),
    FUNCTION_MODIFIER(42),
    FractionalMagnitude(43),
    IDENTICAL_OP(44),
    IF_CLAUSE(45),
    IMPORT(46),
    INCREMENT_OP(47),
    INDEX_OP(48),
    INTERFACE_DEFINITION(49),
    INTERSECTION_OP(50),
    INTERSECT_ASSIGN_OP(51),
    IN_OP(52),
    IS_OP(53),
    IdentifierPart(54),
    IdentifierStart(55),
    LARGER_OP(56),
    LARGE_AS_OP(57),
    LBRACE(58),
    LBRACKET(59),
    LIDENTIFIER(60),
    LINE_COMMENT(61),
    LPAREN(62),
    Letter(63),
    MEMBER_OP(64),
    MULTIPLY_ASSIGN_OP(65),
    MULTI_COMMENT(66),
    Magnitude(67),
    NATURAL_LITERAL(68),
    NONEMPTY(69),
    NOT_EQUAL_OP(70),
    NOT_OP(71),
    NonCharacterChars(72),
    NonStringChars(73),
    OBJECT_DEFINITION(74),
    OR_ASSIGN_OP(75),
    OR_OP(76),
    OUT(77),
    OUTER(78),
    POWER_OP(79),
    PRODUCT_OP(80),
    QMARK(81),
    QUOTED_LITERAL(82),
    QUOTIENT_OP(83),
    QuotedLiteralPart(84),
    RANGE_OP(85),
    RBRACE(86),
    RBRACKET(87),
    REMAINDER_ASSIGN_OP(88),
    REMAINDER_OP(89),
    RETURN(90),
    RPAREN(91),
    SAFE_INDEX_OP(92),
    SAFE_MEMBER_OP(93),
    SATISFIES(94),
    SEMICOLON(95),
    SMALLER_OP(96),
    SMALL_AS_OP(97),
    SPECIFY(98),
    SPREAD_OP(99),
    STRING_LITERAL(100),
    SUBTRACT_ASSIGN_OP(101),
    SUM_OP(102),
    SUPER(103),
    SWITCH_CLAUSE(104),
    StringPart(105),
    THEN_CLAUSE(106),
    THIS(107),
    THROW(108),
    TRY_CLAUSE(109),
    TYPE_CONSTRAINT(110),
    UIDENTIFIER(111),
    UNION_ASSIGN_OP(112),
    UNION_OP(113),
    VALUE_MODIFIER(114),
    VOID_MODIFIER(115),
    WHILE_CLAUSE(116),
    WS(117),
    XOR_ASSIGN_OP(118),
    XOR_OP(119);

    private final int type;
    private IElementType elementType;

    CeylonToken(int type) {
        this.type = type;
    }

    public int getType() {
        return type;
    }

    public IElementType getElementType() {
        if (elementType == null) {
            synchronized (this) {
                if (elementType == null) {
                    elementType = new CeylonElementType(this);
                }
            }
        }
        return elementType;
    }

    public static CeylonToken valueOf(int code) {
        for (CeylonToken tokenType : values()) {
            if (tokenType.type == code) {
                return tokenType;
            }
        }
        return BAD_CHARACTER;
    }

    @Override
    public boolean parseRequired(PsiBuilder builder) {
        return ParserUtils.getToken(builder, getElementType(), this + " expected");
    }

    @Override
    public boolean parseOptional(PsiBuilder builder) {
        return ParserUtils.getToken(builder, getElementType());
    }
}
