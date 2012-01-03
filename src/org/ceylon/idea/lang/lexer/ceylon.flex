package org.ceylon.idea.lang.lexer;
import com.intellij.lexer.FlexLexer;
import com.intellij.psi.tree.IElementType;
import static org.ceylon.idea.lang.lexer.CeylonToken.*;

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

LINE_COMMENT= ("//"|"#!") [^\n\r]*  ("\r\n" | "\r" | "\n")?





COMMENT_TAIL=([^"*"]*("*"+[^"*""/"])?)*("*"+"/")?
MULTI_COMMENT=("/*"[^"*"]{COMMENT_TAIL})|"/*"
STRING_LITERAL=\"([^\\\"\r\n]|{ESCAPE_SEQUENCE})*(\"|\\)?
ESCAPE_SEQUENCE=\\[^\r\n]

%%

<YYINITIAL> {WHITE_SPACE_CHAR}+ { return WS; }
<YYINITIAL> {LINE_COMMENT} { return LINE_COMMENT; }
<YYINITIAL> {MULTI_COMMENT} { return MULTI_COMMENT; }
<YYINITIAL> {STRING_LITERAL} { return STRING_LITERAL; }


<YYINITIAL> "abstracts" { return ABSTRACTED_TYPE; }
<YYINITIAL> "adapts" { return ADAPTED_TYPES; }
<YYINITIAL> "assign" { return ASSIGN; }
<YYINITIAL> "break" { return BREAK; }
<YYINITIAL> "case" { return CASE_CLAUSE; }
<YYINITIAL> "catch" { return CATCH_CLAUSE; }
<YYINITIAL> "class" { return CLASS_DEFINITION; }
<YYINITIAL> "continue" { return CONTINUE; }
<YYINITIAL> "else" { return ELSE_CLAUSE; }
<YYINITIAL> "exists" { return EXISTS; }
<YYINITIAL> "extends" { return EXTENDS; }
<YYINITIAL> "finally" { return FINALLY_CLAUSE; }
<YYINITIAL> "for" { return FOR_CLAUSE; }
<YYINITIAL> "given" { return TYPE_CONSTRAINT; }
<YYINITIAL> "if" { return IF_CLAUSE; }
<YYINITIAL> "satisfies" { return SATISFIES; }
<YYINITIAL> "import" { return IMPORT; }
<YYINITIAL> "interface" { return INTERFACE_DEFINITION; }
<YYINITIAL> "value" { return VALUE_MODIFIER; }
<YYINITIAL> "function" { return FUNCTION_MODIFIER; }
<YYINITIAL> "nonempty" { return NONEMPTY; }
<YYINITIAL> "return" { return RETURN ; }
<YYINITIAL> "super" { return SUPER; }
<YYINITIAL> "switch" { return SWITCH_CLAUSE; }
<YYINITIAL> "then" { return THEN_CLAUSE; }
<YYINITIAL> "this" { return THIS; }
<YYINITIAL> "outer" { return OUTER; }
<YYINITIAL> "object" { return OBJECT_DEFINITION; }
<YYINITIAL> "of" { return CASE_TYPES; }
<YYINITIAL> "out" { return OUT; }
<YYINITIAL> "throw" { return THROW; }
<YYINITIAL> "try" { return TRY_CLAUSE; }
<YYINITIAL> "void" { return VOID_MODIFIER; }
<YYINITIAL> "while" { return WHILE_CLAUSE; }
<YYINITIAL> "..." { return ELLIPSIS; }
<YYINITIAL> ".." { return RANGE_OP; }
<YYINITIAL> "." { return MEMBER_OP; }
<YYINITIAL> "(" { return LPAREN; }
<YYINITIAL> ")" { return RPAREN; }
<YYINITIAL> "{" { return LBRACE; }
<YYINITIAL> "}" { return RBRACE; }
<YYINITIAL> "]" { return RBRACKET; }
<YYINITIAL> ";" { return SEMICOLON; }
<YYINITIAL> "," { return COMMA; }
<YYINITIAL> "=" { return SPECIFY; }
<YYINITIAL> "!" { return NOT_OP; }
<YYINITIAL> "~" { return COMPLEMENT_OP; }
<YYINITIAL> ":=" { return ASSIGN_OP; }
<YYINITIAL> "==" { return EQUAL_OP; }
<YYINITIAL> "===" { return IDENTICAL_OP; }
<YYINITIAL> "&&" { return AND_OP; }
<YYINITIAL> "||" { return OR_OP; }
<YYINITIAL> "++" { return INCREMENT_OP; }
<YYINITIAL> "--" { return DECREMENT_OP; }
<YYINITIAL> "+" { return SUM_OP; }
<YYINITIAL> "-" { return DIFFERENCE_OP; }
<YYINITIAL> "*" { return PRODUCT_OP; }
<YYINITIAL> "/" { return QUOTIENT_OP; }
<YYINITIAL> "&" { return INTERSECTION_OP; }
<YYINITIAL> "|" { return UNION_OP; }
<YYINITIAL> "^" { return XOR_OP; }
<YYINITIAL> "%" { return REMAINDER_OP; }
<YYINITIAL> "!=" { return NOT_EQUAL_OP; }
<YYINITIAL> ">" { return LARGER_OP; }
<YYINITIAL> "<" { return SMALLER_OP; }
<YYINITIAL> ">=" { return LARGE_AS_OP; }
<YYINITIAL> "<=" { return SMALL_AS_OP; }
<YYINITIAL> "->" { return ENTRY_OP; }
<YYINITIAL> "<=>" { return COMPARE_OP; }
<YYINITIAL> "in" { return IN_OP; }
<YYINITIAL> "is" { return IS_OP; }
<YYINITIAL> "**" { return POWER_OP; }
<YYINITIAL> ".=" { return APPLY_OP; }
<YYINITIAL> "+=" { return ADD_ASSIGN_OP; }
<YYINITIAL> "-=" { return SUBTRACT_ASSIGN_OP; }
<YYINITIAL> "*=" { return MULTIPLY_ASSIGN_OP; }
<YYINITIAL> "/=" { return DIVIDE_ASSIGN_OP; }
<YYINITIAL> "&=" { return INTERSECT_ASSIGN_OP; }
<YYINITIAL> "|=" { return UNION_ASSIGN_OP; }
<YYINITIAL> "^=" { return XOR_ASSIGN_OP; }
<YYINITIAL> "~=" { return COMPLEMENT_ASSIGN_OP; }
<YYINITIAL> "%=" { return REMAINDER_ASSIGN_OP; }
<YYINITIAL> "&&=" { return AND_ASSIGN_OP; }
<YYINITIAL> "||=" { return OR_ASSIGN_OP; }
<YYINITIAL> "@" { return COMPILER_ANNOTATION; }

<YYINITIAL> {LIDENTIFIER} { return LIDENTIFIER; }
<YYINITIAL> {UIDENTIFIER} { return UIDENTIFIER; }

<YYINITIAL> .     { return BAD_CHARACTER; }
