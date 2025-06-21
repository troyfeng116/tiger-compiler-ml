type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

fun handleNewLine pos = (
    lineNum := !lineNum + 1;
    linePos := pos :: !linePos
)

val commentDepth = ref 0
fun openComment() = commentDepth := !commentDepth+1;
fun closeComment() = commentDepth := !commentDepth-1;

val curString = ref ""
val curStringStart = ref 0
fun appendCurString s = curString := !curString ^ s

fun eof() = let
    val pos = hd(!linePos)
in
    if !commentDepth <> 0 then ErrorMsg.error pos "unclosed comment" else ();
    Tokens.EOF(pos,pos)
end


(* @see  *)

(*
The escape sequences are
\n      Newline
\t      Tab
\"      Double quote
\\      Backslash
\^c     Control-c, where c is one of @A...Z[\]^_.
\ddd    The character with ASCII code ddd (three decimal digits)
\···\   Any sequence of whitespace characters (spaces, tabs, newlines, returns, and formfeeds)
        surrounded by \s is ignored. This allows string constants to span multiple lines by
        ending and starting each with a backslash
*)

(*
The reserved words of the language are:
while,for,to,break,let,in,end,function,var,type,array,if,then do,of,nil
*)
(* val keywordsPairList : (string * (Tokens.linenum * Tokens.linenum -> Tokens.token)) list = [
    ("while", Tokens.WHILE),
    ("for", Tokens.FOR),
    ("to", Tokens.TO),
    ("break", Tokens.BREAK),
    ("let", Tokens.LET),
    ("in", Tokens.IN),
    ("end", Tokens.END),
    ("function", Tokens.FUNCTION),
    ("var", Tokens.VAR),
    ("type", Tokens.TYPE),
    ("array", Tokens.ARRAY),
    ("if", Tokens.IF),
    ("then", Tokens.THEN),
    ("else", Tokens.ELSE),
    ("do", Tokens.DO),
    ("nil", Tokens.NIL)
] *)

(*
The punctuation symbols used in the language are:
, : ; () [ ] { } . + - * / = <> < <= > >= & | :=
*)

%%
alpha=[A-Za-z];
digit=[0-9];
alphanum=[A-Za-z0-9];
digits={digit}+;
ws = [\ \t];
nl = [\n\r];

%s COMMENT STRING STRING_ESC STRING_WS_ESC STRING_CTRL_ESC;

%%

<INITIAL>{nl}	    => (handleNewLine yypos; continue());

<INITIAL>{ws}       => (continue());

<INITIAL>while      => (Tokens.WHILE(yypos, yypos+size yytext));
<INITIAL>for        => (Tokens.FOR(yypos, yypos+size yytext));
<INITIAL>to         => (Tokens.TO(yypos, yypos+size yytext));
<INITIAL>break      => (Tokens.BREAK(yypos, yypos+size yytext));
<INITIAL>let        => (Tokens.LET(yypos, yypos+size yytext));
<INITIAL>in         => (Tokens.IN(yypos, yypos+size yytext));
<INITIAL>end        => (Tokens.END(yypos, yypos+size yytext));
<INITIAL>function   => (Tokens.FUNCTION(yypos, yypos+size yytext));
<INITIAL>var        => (Tokens.VAR(yypos, yypos+size yytext));
<INITIAL>type       => (Tokens.TYPE(yypos, yypos+size yytext));
<INITIAL>array      => (Tokens.ARRAY(yypos, yypos+size yytext));
<INITIAL>if         => (Tokens.IF(yypos, yypos+size yytext));
<INITIAL>then       => (Tokens.THEN(yypos, yypos+size yytext));
<INITIAL>else       => (Tokens.ELSE(yypos, yypos+size yytext));
<INITIAL>do         => (Tokens.DO(yypos, yypos+size yytext));
<INITIAL>nil        => (Tokens.NIL(yypos, yypos+size yytext));

<INITIAL>","	    => (Tokens.COMMA(yypos, yypos+1));
<INITIAL>":"	    => (Tokens.COLON(yypos, yypos+1));
<INITIAL>";"	    => (Tokens.SEMICOLON(yypos, yypos+1));
<INITIAL>"("	    => (Tokens.LPAREN(yypos, yypos+1));
<INITIAL>")"	    => (Tokens.RPAREN(yypos, yypos+1));
<INITIAL>"["	    => (Tokens.LBRACE(yypos, yypos+1));
<INITIAL>"]"	    => (Tokens.RBRACE(yypos, yypos+1));
<INITIAL>"{"	    => (Tokens.LBRACK(yypos, yypos+1));
<INITIAL>"}"	    => (Tokens.RBRACK(yypos, yypos+1));
<INITIAL>"."        => (Tokens.DOT(yypos, yypos+1));
<INITIAL>"+"	    => (Tokens.PLUS(yypos, yypos+1));
<INITIAL>"-"	    => (Tokens.MINUS(yypos, yypos+1));
<INITIAL>"*"	    => (Tokens.TIMES(yypos, yypos+1));
<INITIAL>"/"	    => (Tokens.DIVIDE(yypos, yypos+1));
<INITIAL>"="	    => (Tokens.EQ(yypos, yypos+1));
<INITIAL>"<>"	    => (Tokens.NEQ(yypos, yypos+2));
<INITIAL>"<"	    => (Tokens.LT(yypos, yypos+1));
<INITIAL>"<="	    => (Tokens.LE(yypos, yypos+2));
<INITIAL>">"	    => (Tokens.GT(yypos, yypos+1));
<INITIAL>">="	    => (Tokens.GE(yypos, yypos+2));
<INITIAL>"&"	    => (Tokens.AND(yypos, yypos+1));
<INITIAL>"|"	    => (Tokens.OR(yypos, yypos+1));
<INITIAL>":="	    => (Tokens.ASSIGN(yypos, yypos+2));

<INITIAL>{alpha}[a-zA-Z0-9_]*   => (Tokens.ID(yytext, yypos, yypos+size yytext));
<INITIAL>{digits}               => (Tokens.INT(valOf(Int.fromString yytext), yypos, yypos+size yytext));

<INITIAL>"\""           => (
    YYBEGIN STRING;
    curString := "";
    curStringStart := yypos+1;
    continue()
);
<STRING>{nl}            => (
    ErrorMsg.error yypos "unclosed string";
    handleNewLine yypos;
    YYBEGIN INITIAL;
    continue()
);
<STRING>"\\"            => (YYBEGIN STRING_ESC; continue());
<STRING>"\""            => (
    YYBEGIN INITIAL;
    Tokens.STRING(!curString, !curStringStart, yypos+1)
);
<STRING>.               => (appendCurString yytext; continue());

<STRING_ESC>n           => (
    appendCurString (String.str #"\n");
    YYBEGIN STRING;
    continue()
);
<STRING_ESC>t           => (
    appendCurString (String.str #"\t");
    YYBEGIN STRING;
    continue()
);
<STRING_ESC>("\""|"\\")     => (
    appendCurString yytext;
    YYBEGIN STRING;
    continue()
);
<STRING_ESC>{ws}        => (YYBEGIN STRING_WS_ESC; continue());
<STRING_ESC>{nl}        => (
    YYBEGIN STRING_WS_ESC;
    handleNewLine yypos;
    continue()
);
<STRING_ESC>"^"         => (YYBEGIN STRING_CTRL_ESC; continue());
<STRING_ESC>{digit}{3}  => (
    let
        val asciiInt = valOf (Int.fromString yytext)
    in (
        if
            asciiInt <= Char.maxOrd
        then
            appendCurString (String.str (chr asciiInt))
        else ErrorMsg.error yypos ("invalid ascii escape sequence value " ^ yytext)
    ) end;
    YYBEGIN STRING;
    continue()
);
<STRING_ESC>.           => (
    ErrorMsg.error yypos ("illegal string escape " ^ yytext);
    YYBEGIN STRING;
    continue()
);
<STRING_WS_ESC>{ws}     => (continue());
<STRING_WS_ESC>{nl}     => (handleNewLine yypos; continue());
<STRING_WS_ESC>"\\"     => (YYBEGIN STRING; continue());
<STRING_WS_ESC>.        => (
    ErrorMsg.error yypos ("illegal string whitespace escape " ^ yytext);
    YYBEGIN STRING;
    continue()
);
<STRING_CTRL_ESC>(@|[A-Z]|"\["|"\\"|"\]"|"^"|_|"\.")    => (
    appendCurString ("\\^" ^ yytext);
    YYBEGIN STRING;
    continue()
);
<STRING_CTRL_ESC>"\""       => (
    ErrorMsg.error yypos ("unfinished ctrl sequence");
    YYBEGIN INITIAL;
    continue()
);
<STRING_CTRL_ESC>.          => (
    ErrorMsg.error yypos ("illegal ctrl sequence " ^ yytext);
    YYBEGIN STRING;
    continue()
);

<INITIAL>"/*"           => (
    openComment();
    YYBEGIN COMMENT;
    continue()
);
<COMMENT>"/*"           => (openComment(); continue());
<COMMENT>{nl}           => (handleNewLine yypos; continue());
<COMMENT>"*/"           => (
    closeComment();
    if !commentDepth = 0 then YYBEGIN INITIAL else ();
    continue()
);
<COMMENT>.              => (continue());

<INITIAL>.       => (
    ErrorMsg.error yypos ("illegal character " ^ yytext);
    continue()
);

