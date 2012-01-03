package org.ceylon.idea;

import com.intellij.psi.PsiFile;
import com.intellij.psi.impl.DebugUtil;
import com.intellij.testFramework.fixtures.LightCodeInsightFixtureTestCase;

import java.util.List;

public abstract class CeylonTestCase extends LightCodeInsightFixtureTestCase {

    @Override
    protected String getBasePath() {
        return TestUtils.getTestDataPath() + "parsing/";
    }

    public void doTest() {
        doTest(getTestName(true).replace('$', '/') + ".test");
    }

    protected void doTest(String fileName) {
        final List<String> list = TestUtils.readInput(getBasePath() + "/" + fileName);

        final String input = list.get(0);
        final String output = list.get(1);
        checkParsing(input, output);
    }

    protected void checkParsing(String input, String output) {
        final PsiFile psiFile = TestUtils.createPseudoPhysicalCeylonFile(getProject(), input);
        String psiTree = DebugUtil.psiToString(psiFile, false, true);
        assertEquals(output.trim(), psiTree.trim());
    }
}
