package org.ceylon.idea.lang.lexer;
import com.intellij.lexer.FlexLexer;
import com.intellij.psi.JavaTokenType;
import com.intellij.psi.tree.IElementType;
import static org.ceylon.idea.lang.lexer.CeylonTokenType.*;

@SuppressWarnings({"ALL"})
%%

%unicode
%class _CeylonLexer
%implements FlexLexer
%function advance
%type IElementType
%eof{  return;
%eof}

LIDENTIFIER=[:lowercase:] [:jletterdigit:]*
UIDENTIFIER=[:uppercase:] [:jletterdigit:]*
WHITE_SPACE_CHAR=[\ \n\r\t\f]
COMMENT_TAIL=([^"*"]*("*"+[^"*""/"])?)*("*"+"/")?
C_STYLE_COMMENT=("/*"[^"*"]{COMMENT_TAIL})|"/*"
STRING_LITERAL=\"([^\\\"\r\n]|{ESCAPE_SEQUENCE})*(\"|\\)?
ESCAPE_SEQUENCE=\\[^\r\n]

%%

<YYINITIAL> {WHITE_SPACE_CHAR}+ { return WHITE_SPACE; }
<YYINITIAL> {C_STYLE_COMMENT} { return C_STYLE_COMMENT; }
<YYINITIAL> {STRING_LITERAL} { return STRING_LITERAL; }

<YYINITIAL> "abstracts" { return ABSTRACTS_KEYWORD; }
<YYINITIAL> "adapts" { return ADAPTS_KEYWORD; }
<YYINITIAL> "assign" { return ASSIGN_KEYWORD; }
<YYINITIAL> "break" { return BREAK_KEYWORD; }
<YYINITIAL> "case" { return CASE_KEYWORD; }
<YYINITIAL> "catch" { return CATCH_KEYWORD; }
<YYINITIAL> "class" { return CLASS_KEYWORD; }
<YYINITIAL> "continue" { return CONTINUE_KEYWORD; }
<YYINITIAL> "else" { return ELSE_KEYWORD; }
<YYINITIAL> "exists" { return EXISTS_KEYWORD; }
<YYINITIAL> "extends" { return EXTENDS_KEYWORD; }
<YYINITIAL> "finally" { return FINALLY_KEYWORD; }
<YYINITIAL> "for" { return FOR_KEYWORD; }
<YYINITIAL> "function" { return FUNCTION_KEYWORD; }
<YYINITIAL> "given" { return GIVEN_KEYWORD; }
<YYINITIAL> "if" { return IF_KEYWORD; }
<YYINITIAL> "import" { return IMPORT_KEYWORD; }
<YYINITIAL> "in" { return IN_KEYWORD; }
<YYINITIAL> "interface" { return INTERFACE_KEYWORD; }
<YYINITIAL> "is" { return IS_KEYWORD; }
<YYINITIAL> "nonempty" { return NONEMPTY_KEYWORD; }
<YYINITIAL> "object" { return OBJECT_KEYWORD; }
<YYINITIAL> "of" { return OF_KEYWORD; }
<YYINITIAL> "out" { return OUT_KEYWORD; }
<YYINITIAL> "outer" { return OUTER_KEYWORD; }
<YYINITIAL> "return" { return RETURN_KEYWORD; }
<YYINITIAL> "satisfies" { return SATISFIES_KEYWORD; }
<YYINITIAL> "super" { return SUPER_KEYWORD; }
<YYINITIAL> "switch" { return SWITCH_KEYWORD; }
<YYINITIAL> "then" { return THEN_KEYWORD; }
<YYINITIAL> "this" { return THIS_KEYWORD; }
<YYINITIAL> "throw" { return THROW_KEYWORD; }
<YYINITIAL> "try" { return TRY_KEYWORD; }
<YYINITIAL> "value" { return VALUE_KEYWORD; }
<YYINITIAL> "void" { return VOID_KEYWORD; }
<YYINITIAL> "while" { return WHILE_KEYWORD; }

<YYINITIAL> {LIDENTIFIER} { return LIDENTIFIER; }
<YYINITIAL> {UIDENTIFIER} { return UIDENTIFIER; }

<YYINITIAL> "("   { return JavaTokenType.LPARENTH; }
<YYINITIAL> ")"   { return JavaTokenType.RPARENTH; }
<YYINITIAL> "{"   { return JavaTokenType.LBRACE; }
<YYINITIAL> "}"   { return JavaTokenType.RBRACE; }
<YYINITIAL> "["   { return JavaTokenType.LBRACKET; }
<YYINITIAL> "]"   { return JavaTokenType.RBRACKET; }
<YYINITIAL> ";"   { return JavaTokenType.SEMICOLON; }
<YYINITIAL> ","   { return JavaTokenType.COMMA; }
<YYINITIAL> "..." { return JavaTokenType.ELLIPSIS; }
<YYINITIAL> "."   { return JavaTokenType.DOT; }

<YYINITIAL> .     { return BAD_CHARACTER; }
