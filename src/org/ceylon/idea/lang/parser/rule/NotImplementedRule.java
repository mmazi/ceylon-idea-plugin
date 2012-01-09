package org.ceylon.idea.lang.parser.rule;

import com.intellij.lang.PsiBuilder;

public class NotImplementedRule implements Rule {
    private final String name;

/*
    public NotImplementedRule() {
        name = "(UNKNOWN)";
    }
*/

    public NotImplementedRule(String name) {
        this.name = name;
    }

    @Override
    public boolean parseRequired(PsiBuilder builder) {
        throw new RuntimeException(name + " rule is not implemented yet");
    }

    @Override
    public boolean parseOptional(PsiBuilder builder) {
        throw new RuntimeException(name + " rule is not implemented yet");
    }

    @Override
    public String toString() {
        return name;
    }
}
