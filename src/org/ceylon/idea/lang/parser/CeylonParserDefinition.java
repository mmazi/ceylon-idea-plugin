package org.ceylon.idea.lang.parser;

import com.intellij.lang.ASTNode;
import com.intellij.lang.ParserDefinition;
import com.intellij.lang.PsiParser;
import com.intellij.lexer.Lexer;
import com.intellij.openapi.project.Project;
import com.intellij.psi.FileViewProvider;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.tree.IFileElementType;
import com.intellij.psi.tree.TokenSet;
import org.ceylon.idea.lang.lexer.CeylonElementType;
import org.ceylon.idea.lang.lexer.CeylonLexerAdapter;
import org.ceylon.idea.lang.psi.CeylonFile;
import org.ceylon.idea.lang.psi.CeylonPsiCreator;
import org.jetbrains.annotations.NotNull;

public class CeylonParserDefinition implements ParserDefinition {
    @NotNull
    @Override
    public Lexer createLexer(Project project) {
        return new CeylonLexerAdapter();
    }

    @Override
    public PsiParser createParser(Project project) {
        return new CeylonParser();
    }

    @Override
    public IFileElementType getFileNodeType() {
        return CeylonAstNode.FILE;
    }

    @NotNull
    @Override
    public TokenSet getWhitespaceTokens() {
        return CeylonElementType.WHITE_SPACE_SET;
    }

    @NotNull
    @Override
    public TokenSet getCommentTokens() {
        return CeylonElementType.COMMENT_SET;
    }

    @NotNull
    @Override
    public TokenSet getStringLiteralElements() {
        return CeylonElementType.STRING_LITERAL_SET;
    }

    @NotNull
    @Override
    public PsiElement createElement(ASTNode astNode) {
        return CeylonPsiCreator.createElement(astNode);
    }

    @Override
    public PsiFile createFile(FileViewProvider fileViewProvider) {
        return new CeylonFile(fileViewProvider);
    }

    @Override
    public SpaceRequirements spaceExistanceTypeBetweenTokens(ASTNode astNode, ASTNode astNode1) {
        return SpaceRequirements.MAY;
    }
}
