package org.ceylon.idea.lang.parser.rule;

import com.intellij.lang.PsiBuilder;

class OneOrMoreRule implements Rule {
    private final Rule rule;

    OneOrMoreRule(Rule rule) {
        this.rule = rule;
    }

    OneOrMoreRule(Rule first, Rule second, Rule... rules) {
        this(new SequenceRule(first, second, rules));
    }

    @Override
    public boolean parseRequired(PsiBuilder builder) {
        boolean parsed = rule.parseRequired(builder);
        if (!parsed) {
            return false;
        }

        while (rule.parseRequired(builder)) {
        }

        return true;
    }

    // TODO: get rid of this copy&pase
    @Override
    public boolean parseOptional(PsiBuilder builder) {
        boolean parsed = rule.parseOptional(builder);
        if (!parsed) {
            return false;
        }

        while (rule.parseOptional(builder)) {
        }

        return true;
    }

    @Override
    public String toString() {
        return rule + "+";
    }
}
