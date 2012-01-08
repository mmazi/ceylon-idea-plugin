package org.ceylon.idea.lang.parser.rule;

import com.intellij.lang.PsiBuilder;

import static org.ceylon.idea.lang.parser.rule.Rules.sequence;

class ZeroOrMoreRule implements Rule {
    private final Rule rule;
    private final String name;

    ZeroOrMoreRule(Rule rule) {
        this.rule = rule;
        name = rule.toString();
    }

    ZeroOrMoreRule(Rule first, Rule second, Rule... rules) {
        this(sequence(first, second, rules));
    }

    @Override
    public boolean parseRequired(PsiBuilder builder) {
        while (lookAhead(builder)) {
            if (!rule.parseRequired(builder)) {
                throw new IllegalStateException("Failed to parse " + rule);
            }
        }
        return true;
    }

    // TODO: get rid of this copy&pase
    @Override
    public boolean parseOptional(PsiBuilder builder) {
        while (lookAhead(builder)) {
            if (!rule.parseOptional(builder)) {
                throw new IllegalStateException("Failed to parse " + rule);
            }
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
        return name + "*";
    }
}
