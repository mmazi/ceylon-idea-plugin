package org.ceylon.idea.lang.parser.rule;

import com.intellij.lang.PsiBuilder;
import org.apache.commons.lang.StringUtils;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

class ZeroOrAnyRule implements Rule {
    private final List<Rule> rules = new LinkedList<Rule>();

    ZeroOrAnyRule(Rule first, Rule... rest) {
        rules.add(first);
        rules.addAll(Arrays.asList(rest));
    }

    @Override
    public boolean parseRequired(PsiBuilder builder) {
        // We shouldn't report errors because we don't know which particular case is required
        return parseOptional(builder);
    }

    @Override
    public boolean parseOptional(PsiBuilder builder) {
        for (Rule rule : rules) {
            boolean parsed = rule.parseOptional(builder);
            if (parsed) {
                return true;
            }
        }

        return true;
    }

    @Override
    public String toString() {
        return "(" + StringUtils.join(rules, " | ") + ")?";
    }
}
