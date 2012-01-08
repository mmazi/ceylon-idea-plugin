package org.ceylon.idea.lang.parser.rule;

import com.intellij.lang.PsiBuilder;
import org.ceylon.idea.lang.parser.CeylonAstNode;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

class ComplexRule implements Rule {
    private final String name;
    private final List<Rule> rules = new LinkedList<Rule>();

    public static ComplexRule rule(String name) {
        return new ComplexRule(name);
    }

    public ComplexRule(String name) {
        this.name = name;
    }

    public boolean parseRequired(PsiBuilder builder) {
        if (rules.isEmpty()) {
            throw new IllegalStateException("No rules defined for " + name);
        }

        PsiBuilder.Marker marker = builder.mark();

        for (Rule rule : rules) {
            boolean parsed = rule.parseRequired(builder);
            if (!parsed) {
                marker.rollbackTo();
                return false;
            }
        }
        marker.done(new CeylonAstNode(name));
        return true;
    }

    @Override
    public boolean parseOptional(PsiBuilder builder) {
        if (rules.isEmpty()) {
            throw new IllegalStateException("No rules defined for " + name);
        }

        PsiBuilder.Marker marker = builder.mark();
        for (Rule rule : rules) {
            boolean parsed = rule.parseOptional(builder);
            if (!parsed) {
                marker.rollbackTo();
                return false;
            }
        }
        marker.done(new CeylonAstNode(name));
        return true;
    }

    public ComplexRule one(Rule rule) {
        rules.add(rule);
        return this;
    }

    public ComplexRule oneOrMore(Rule rule) {
        rules.add(new OneOrMoreRule(rule));
        return this;
    }

    public ComplexRule zeroOrOne(Rule rule) {
        this.rules.add(new ZeroOrOneRule(rule));
        return this;
    }

    public ComplexRule zeroOrOne(Rule first, Rule second, Rule... rest) {
        this.rules.add(new ZeroOrOneRule(first, second, rest));
        return this;
    }

    public ComplexRule zeroOrMore(Rule rule) {
        rules.add(new ZeroOrMoreRule(rule));
        return this;
    }

    public ComplexRule zeroOrMore(Rule first, Rule second, Rule... rest) {
        rules.add(new ZeroOrMoreRule(first, second, rest));
        return this;
    }

    public ComplexRule any(Rule first, Rule... rest) {
        rules.add(new AnyRule(first, rest));
        return this;
    }

    public ComplexRule sequence(Rule first, Rule second, Rule... rest) {
        rules.add(first);
        rules.add(second);
        rules.addAll(Arrays.asList(rest));
        return this;
    }

    public String toString() {
        return name;
    }
}
