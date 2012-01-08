package org.ceylon.idea.lang.parser.rule;

import com.intellij.lang.PsiBuilder;

class OneOrMoreRule implements Rule {
    private final Rule rule;

    OneOrMoreRule(Rule rule) {
        this.rule = rule;
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

    @Override
    public boolean parseOptional(PsiBuilder builder) {
        return rule.parseOptional(builder);
    }

    @Override
    public String toString() {
        return rule + "+";
    }
}
