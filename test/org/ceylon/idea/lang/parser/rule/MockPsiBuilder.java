package org.ceylon.idea.lang.parser.rule;

import com.intellij.lang.*;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Key;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.TokenSet;
import com.intellij.util.diff.FlyweightCapableTreeStructure;
import org.ceylon.idea.lang.lexer.CeylonToken;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class MockPsiBuilder implements PsiBuilder {
    private int position = 0;
    private CeylonToken[] tokens = {};
    private IElementType node;

    public void setTokens(CeylonToken... tokens) {
        this.tokens = tokens;
        position = 0;
    }

    public IElementType getNode() {
        return node;
    }

    @Override
    public Project getProject() {
        throw new UnsupportedOperationException();
    }

    @Override
    public CharSequence getOriginalText() {
        throw new UnsupportedOperationException();
    }

    @Override
    public void advanceLexer() {
        position++;
    }

    @Override
    public IElementType getTokenType() {
        return eof() ? null : tokens[position].getElementType();
    }

    public CeylonToken getToken() {
        return eof() ? null : tokens[position];
    }

    @Override
    public void setTokenTypeRemapper(@Nullable ITokenTypeRemapper iTokenTypeRemapper) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void remapCurrentToken(IElementType iElementType) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void setWhitespaceSkippedCallback(WhitespaceSkippedCallback whitespaceSkippedCallback) {
        throw new UnsupportedOperationException();
    }

    @Override
    public IElementType lookAhead(int i) {
        return tokens[position + i].getElementType();
    }

    @Override
    public IElementType rawLookup(int i) {
        throw new UnsupportedOperationException();
    }

    @Override
    public int rawTokenTypeStart(int i) {
        throw new UnsupportedOperationException();
    }

    @Override
    public String getTokenText() {
        throw new UnsupportedOperationException();
    }

    @Override
    public int getCurrentOffset() {
        throw new UnsupportedOperationException();
    }

    @Override
    public Marker mark() {
        return new MockMarker();
    }

    @Override
    public void error(String s) {
        // TODO
    }

    @Override
    public boolean eof() {
        return position >= tokens.length;
    }

    @Override
    public ASTNode getTreeBuilt() {
        throw new UnsupportedOperationException();
    }

    @Override
    public FlyweightCapableTreeStructure<LighterASTNode> getLightTree() {
        throw new UnsupportedOperationException();
    }

    @Override
    public void setDebugMode(boolean b) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void enforceCommentTokens(TokenSet tokenSet) {
        throw new UnsupportedOperationException();
    }

    @Override
    public LighterASTNode getLatestDoneMarker() {
        throw new UnsupportedOperationException();
    }

    @Override
    public <T> T getUserData(@NotNull Key<T> tKey) {
        throw new UnsupportedOperationException();
    }

    @Override
    public <T> void putUserData(@NotNull Key<T> tKey, @Nullable T t) {
        throw new UnsupportedOperationException();
    }

    @Override
    public <T> T getUserDataUnprotected(@NotNull Key<T> tKey) {
        throw new UnsupportedOperationException();
    }

    @Override
    public <T> void putUserDataUnprotected(@NotNull Key<T> tKey, @Nullable T t) {
        throw new UnsupportedOperationException();
    }


    private class MockMarker implements Marker {
        private final int markedPosition;

        private MockMarker() {
            this.markedPosition = position;
        }

        @Override
        public Marker precede() {
            throw new UnsupportedOperationException();
        }

        @Override
        public void drop() {
            // TODO
        }

        @Override
        public void rollbackTo() {
            position = markedPosition;
        }

        @Override
        public void done(IElementType node) {
            MockPsiBuilder.this.node = node;
        }

        @Override
        public void collapse(IElementType iElementType) {
            throw new UnsupportedOperationException();
        }

        @Override
        public void doneBefore(IElementType iElementType, Marker marker) {
            throw new UnsupportedOperationException();
        }

        @Override
        public void doneBefore(IElementType iElementType, Marker marker, String s) {
            throw new UnsupportedOperationException();
        }

        @Override
        public void error(String s) {
            throw new UnsupportedOperationException();
        }

        @Override
        public void errorBefore(String s, Marker marker) {
            throw new UnsupportedOperationException();
        }

        @Override
        public void setCustomEdgeTokenBinders(@Nullable WhitespacesAndCommentsBinder whitespacesAndCommentsBinder, @Nullable WhitespacesAndCommentsBinder whitespacesAndCommentsBinder1) {
            throw new UnsupportedOperationException();
        }
    }
}
