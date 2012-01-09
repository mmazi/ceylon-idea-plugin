package org.ceylon.idea.lang.parser.rule;

public class Rules {
    public static ComplexRule rule(String name) {
        return new ComplexRule(name);
    }

    public static Rule any(Rule first, Rule second, Rule... rest) {
        return new AnyRule(first, second, rest);
    }

    public static Rule sequence(Rule first, Rule second, Rule... rest) {
        return new SequenceRule(first, second, rest);
    }

    public static Rule oneOrMore(Rule rule) {
        return new OneOrMoreRule(rule);
    }

    public static Rule oneOrMore(Rule first, Rule second, Rule... rest) {
        return new OneOrMoreRule(first, second, rest);
    }

    public static Rule zeroOrOne(Rule rule) {
        return new ZeroOrOneRule(rule);
    }

    public static Rule zeroOrOne(Rule first, Rule second, Rule... rest) {
        return new ZeroOrOneRule(first, second, rest);
    }

    public static Rule zeroOrAny(Rule first, Rule... rest) {
        return new ZeroOrAnyRule(first, rest);
    }

    public static Rule zeroOrMore(Rule rule) {
        return new ZeroOrMoreRule(rule);
    }

    public static Rule zeroOrMore(Rule first, Rule second, Rule... rules) {
        return new ZeroOrMoreRule(first, second, rules);
    }
}
