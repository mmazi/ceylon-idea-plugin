package org.ceylon.idea.lang.parser.rule;

import com.intellij.lang.PsiBuilder;

import static org.ceylon.idea.lang.parser.rule.SequenceRule.sequence;

class ZeroOrOneRule implements Rule {
    private final Rule rule;

    public static Rule zeroOrOne(Rule rule) {
        return new ZeroOrOneRule(rule);
    }

    public ZeroOrOneRule(Rule first, Rule second, Rule[] rest) {
        this(sequence(first, second, rest));
    }

    public static Rule zeroOrOne(Rule first, Rule second, Rule... rules) {
        return zeroOrOne(sequence(first, second, rules));
    }

    ZeroOrOneRule(Rule rule) {
        this.rule = rule;
    }

    @Override
    public boolean parseRequired(PsiBuilder builder) {
        if (lookAhead(builder)) {
            if (!rule.parseRequired(builder)) {
                throw new IllegalStateException("Failed to parse " + rule);
            }
        }
        return true;
    }

    @Override
    public boolean parseOptional(PsiBuilder builder) {
        if (lookAhead(builder)) {
            rule.parseOptional(builder);
        }
        return true;
    }

    private boolean lookAhead(PsiBuilder builder) {
        PsiBuilder.Marker marker = builder.mark();
        boolean parsed = rule.parseOptional(builder);
        marker.rollbackTo();
        return parsed;
    }

    @Override
    public String toString() {
        return rule + "?";
    }
}
