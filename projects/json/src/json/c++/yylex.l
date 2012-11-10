%{
#define TOK_NULL 1
#define TOK_NAME 2
#define TOK_INTEGER 3
#define TOK_FLOAT 4
#define TOK_STRING 5
#define TOK_LBRACE 6
#define TOK_RBRACE 7
#define TOK_LBRACK 8
#define TOK_RBRACK 9
#define TOK_LPAREN 10
#define TOK_RPAREN 11
#define TOK_COLON 12
#define TOK_COMMA 13

static int const fake_name = TOK_NAME;
static int const fake_int = TOK_INTEGER;
static int const fake_float = TOK_FLOAT;
static int const fake_string = TOK_STRING;

typedef int YYSTYPE;
typedef int YYLTYPE;
%}

%option nounput noinput nodefault noyywrap nounistd
%option 8bit batch full
%option yylineno
%option warn
%option perf-report perf-report
%option bison-locations

ident	([a-zA-Z_][0-9a-zA-Z_]*)

dstring	\"(\\.|[^"\\\n])*\"
sstring	\'(\\.|[^'\\\n])*\'

d	[0-9]

ws	[ \t\r\n]

%%

 /* Whitespace */
{ws}+					{ }

 /* Identifier */
"null"					{ return TOK_NULL; }
{ident}					{ return fake_name /* TOK_NAME (Lexing.lexeme lexbuf) */; }

 /* Integer */
{d}+					{ return fake_int /* TOK_INTEGER (int_of_string (Lexing.lexeme lexbuf)) */; }

 /* Float */
{d}+"."{d}+				{ return fake_float /* TOK_FLOAT (float_of_string (Lexing.lexeme lexbuf)) */; }

 /* String/character */
{dstring}|{sstring}			{ return fake_string /* TOK_STRING (parse_string (Lexing.lexeme lexbuf)) */; }

 /* Punctuators */
"{"					{ return TOK_LBRACE; }
"}"					{ return TOK_RBRACE; }
"["					{ return TOK_LBRACK; }
"]"					{ return TOK_RBRACK; }
"("					{ return TOK_LPAREN; }
")"					{ return TOK_RPAREN; }
":"					{ return TOK_COLON; }
","					{ return TOK_COMMA; }

.					{ puts (yytext); abort (); }

%%

int main ()
{
  while (yylex (0, 0))
    ;

  return 0;
}
