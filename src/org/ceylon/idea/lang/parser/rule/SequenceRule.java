package org.ceylon.idea.lang.parser.rule;

import com.intellij.lang.PsiBuilder;
import org.apache.commons.lang.StringUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

class SequenceRule implements Rule {
    private final List<Rule> rules = new ArrayList<Rule>();

    public static Rule sequence(Rule first, Rule second, Rule... rest) {
        return new SequenceRule(first, second, rest);
    }

    SequenceRule(Rule first, Rule second, Rule... rest) {
        rules.add(first);
        rules.add(second);
        rules.addAll(Arrays.asList(rest));
    }

    @Override
    public boolean parseRequired(PsiBuilder builder) {
        for (Rule rule : rules) {
            boolean parsed = rule.parseRequired(builder);
            if (!parsed) {
                return false;
            }
        }
        return true;
    }

    @Override
    public boolean parseOptional(PsiBuilder builder) {
        for (Rule rule : rules) {

            boolean parsed = rule.parseOptional(builder);
            if (!parsed) {
                return false;
            }
        }
        return true;
    }

    @Override
    public String toString() {
        return "(" + StringUtils.join(rules, " ") + ")";

    }
}