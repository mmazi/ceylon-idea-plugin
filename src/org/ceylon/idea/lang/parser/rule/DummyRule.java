package org.ceylon.idea.lang.parser.rule;

import com.intellij.lang.PsiBuilder;

public class DummyRule extends NotImplementedRule {
    public DummyRule(String name) {
        super(name);
    }

    @Override
    public boolean parseOptional(PsiBuilder builder) {
        return false;
    }

    @Override
    public boolean parseRequired(PsiBuilder builder) {
        return false;
    }
}
