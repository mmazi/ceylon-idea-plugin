package org.ceylon.idea.lang.parser.rule;

import com.intellij.lang.PsiBuilder;

import static org.ceylon.idea.lang.parser.rule.Rules.sequence;

class ZeroOrOneRule implements Rule {
    private final Rule rule;

    ZeroOrOneRule(Rule rule) {
        this.rule = rule;
    }

    ZeroOrOneRule(Rule first, Rule second, Rule[] rest) {
        this(sequence(first, second, rest));
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
