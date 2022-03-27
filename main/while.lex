structure  Tokens = Tokens
type pos = int
type svalue = Tokens.svalue
type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue, pos) token
exception badCharacter
val pos = ref 1
val col = ref 1
val eof = fn () => (Tokens.EOF(!pos, !pos))

%%

%header (functor whileLexFun (structure Tokens : while_TOKENS));
identifier = [a-zA-Z][a-zA-Z0-9]*;
number = [1-9][0-9]*;
whiteSpace = [\ \t\r\n]+;

%%
[+~]?{number}    =>      (col := !col + size(yytext); print("NUM " ^ yytext ^ "\n"); Tokens.NUM(List.foldl (fn (a,r) => ord(a) - ord(#"0") + 10*r) 0 (explode yytext), !pos, !pos));

"~"         =>      (col := !col + 1; print("NEGATE " ^ yytext ^ "\n");  Tokens.NEGATE(!pos, !pos));
";"         =>      (col := !col + 1; print("TERM " ^ yytext ^ "\n");  Tokens.TERM(!pos, !pos));
"::"        =>      (col := !col + 2; print("DCOLON " ^ yytext ^ "\n"); Tokens.DCOLON(!pos, !pos));
":"         =>      (col := !col + 1; print("COLON " ^ yytext ^ "\n"); Tokens.COLON(!pos, !pos));
":="        =>      (col := !col + 2; print("SET " ^ yytext ^ "\n"); Tokens.SET(!pos, !pos));
","         =>      (col := !col + 1; print("COMMA " ^ yytext ^ "\n");  Tokens.COMMA(!pos, !pos));

"{"         =>      (col := !col + 1; print("LBRACE " ^ yytext ^ "\n");  Tokens.LBRACE(!pos, !pos));
"}"         =>      (col := !col + 1; print("RBRACE " ^ yytext ^ "\n");  Tokens.RBRACE(!pos, !pos));
"("         =>      (col := !col + 1; print("LPAREN " ^ yytext ^ "\n");  Tokens.LPAREN(!pos, !pos));
")"         =>      (col := !col + 1; print("RPAREN " ^ yytext ^ "\n");  Tokens.RPAREN(!pos, !pos));

"+"         =>      (col := !col + 1; print("PLUS " ^ yytext ^ "\n");  Tokens.PLUS(!pos, !pos));
"-"         =>      (col := !col + 1; print("MINUS " ^ yytext ^ "\n");  Tokens.MINUS(!pos, !pos));
"*"         =>      (col := !col + 1; print("TIMES " ^ yytext ^ "\n");  Tokens.TIMES(!pos, !pos));
"/"         =>      (col := !col + 1; print("DIV " ^ yytext ^ "\n");  Tokens.DIV(!pos, !pos));
"%"         =>      (col := !col + 1; print("MOD " ^ yytext ^ "\n");  Tokens.MOD(!pos, !pos));

"program"   =>      (col := !col + 7; print("PROG " ^ yytext ^ "\n");  Tokens.PROG(!pos, !pos));
"var"       =>      (col := !col + 3; print("VAR " ^ yytext ^ "\n");  Tokens.VAR(!pos, !pos));
"int"       =>      (col := !col + 3; print("INT " ^ yytext ^ "\n");  Tokens.INT(!pos, !pos));
"bool"      =>      (col := !col + 4; print("BOOL " ^ yytext ^ "\n");  Tokens.BOOL(!pos, !pos));

"read"      =>      (col := !col + 4; print("READ " ^ yytext ^ "\n");  Tokens.READ(!pos, !pos));
"write"     =>      (col := !col + 5; print("WRITE " ^ yytext ^ "\n");  Tokens.WRITE(!pos, !pos));

"if"        =>      (col := !col + 2; print("ITE " ^ yytext ^ "\n");  Tokens.IF(!pos, !pos));
"then"      =>      (col := !col + 4; print("ITE " ^ yytext ^ "\n");  Tokens.THEN(!pos, !pos));
"else"      =>      (col := !col + 4; print("ITE " ^ yytext ^ "\n");  Tokens.ELSE(!pos, !pos));
"endif"     =>      (col := !col + 5; print("ITE " ^ yytext ^ "\n");  Tokens.ENDIF(!pos, !pos));
"while"     =>      (col := !col + 5; print("WH " ^ yytext ^ "\n");  Tokens.WHILE(!pos, !pos));
"do"        =>      (col := !col + 2; print("WH " ^ yytext ^ "\n");  Tokens.DO(!pos, !pos));
"endwh"     =>      (col := !col + 5; print("WH " ^ yytext ^ "\n");  Tokens.ENDWH(!pos, !pos));

"tt"        =>      (col := !col + 2; print("TT " ^ yytext ^ "\n");  Tokens.TT(!pos, !pos));
"ff"        =>      (col := !col + 2; print("FF " ^ yytext ^ "\n");  Tokens.FF(!pos, !pos));
"!"         =>      (col := !col + 1; print("NOT " ^ yytext ^ "\n");  Tokens.NOT(!pos, !pos));
"&&"        =>      (col := !col + 2; print("AND " ^ yytext ^ "\n");  Tokens.AND(!pos, !pos));
"||"        =>      (col := !col + 2; print("OR " ^ yytext ^ "\n");  Tokens.OR(!pos, !pos));

"<>"        =>      (col := !col + 2; print("NEQ " ^ yytext ^ "\n");  Tokens.NEQ(!pos, !pos));
"<"         =>      (col := !col + 1; print("LT " ^ yytext ^ "\n");  Tokens.LT(!pos, !pos));
"<="        =>      (col := !col + 2; print("LEQ " ^ yytext ^ "\n");  Tokens.LEQ(!pos, !pos));
"="         =>      (col := !col + 1; print("EQ " ^ yytext ^ "\n");  Tokens.EQ(!pos, !pos));
">"         =>      (col := !col + 1; print("GT " ^ yytext ^ "\n");  Tokens.GT(!pos, !pos));
">="        =>      (col := !col + 2; print("GEQ " ^ yytext ^ "\n");  Tokens.GEQ(!pos, !pos));

{identifier}=>      (col := !col + size(yytext); print("ID " ^ yytext ^ "\n");  Tokens.ID(yytext, !pos, !pos));
{whiteSpace}=>     (col := !col + size yytext; lex());
.           =>      (raise badCharacter; lex());