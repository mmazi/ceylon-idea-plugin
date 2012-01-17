package org.ceylon.idea.lang.parser.rule;

import com.intellij.lang.PsiBuilder;
import org.apache.commons.lang.StringUtils;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

class AnyRule implements Rule {

    private final List<Rule> rules = new LinkedList<Rule>();

    AnyRule(Rule first, Rule second, Rule... rest) {
        rules.add(first);
        rules.add(second);
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
            PsiBuilder.Marker marker = builder.mark();
            boolean parsed = rule.parseOptional(builder);
            if (parsed) {
                marker.drop();
                return true;
            } else {
                marker.rollbackTo();
            }
        }

        return false;
    }

    @Override
    public String toString() {
        return "(" + StringUtils.join(rules, " | ") + ")";
    }
}
