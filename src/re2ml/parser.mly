%{
  open Ast

  let identity a = a
%}

%token EOF

/* ===================== tokens ============================ */
/* tokens that have many lexical spellings */
%token <char> TOK_ERROR
%token <int> TOK_INTEGER
%token <string Sloc.t> TOK_UNAME
%token <string Sloc.t> TOK_LNAME
%token <string Sloc.t> TOK_CHAR
%token <string Sloc.t> TOK_STRING
%token <string Sloc.t> TOK_LIT_CODE

/* punctuators */
%token TOK_CARET		/* "^"	*/
%token TOK_QUESTION		/* "?"	*/
%token TOK_STAR			/* "*"	*/
%token TOK_PLUS			/* "+"	*/
%token TOK_PIPE			/* "|"	*/
%token TOK_MINUS		/* "-"	*/
%token TOK_EQUALS		/* "="	*/
%token TOK_COMMA		/* ","	*/
%token TOK_LBRACK		/* "["	*/
%token TOK_RBRACK		/* "]"	*/
%token TOK_LBRACE		/* "{"	*/
%token TOK_RBRACE		/* "}"	*/
%token TOK_LPAREN		/* "("	*/
%token TOK_RPAREN		/* ")"	*/

/* keywords */
%token TOK_UNDERLINE		/* "_"		*/
%token TOK_RULE			/* "rule"	*/
%token TOK_LET			/* "let"	*/
%token TOK_AND			/* "and"	*/
%token TOK_PARSE		/* "parse"	*/
%token TOK_AS			/* "as"		*/


/* ===================== productions ======================= */

%start <Ast.t> parse
%%

/* The actions in this file simply build an Abstract Syntax Tree (AST)
 * for later processing. */


/* start symbol */
parse
	: code? lexeme* lexers code? EOF		{ Program ($1, $2, $3, $4) }


code
	: TOK_LIT_CODE					{ $1 }


lexeme
	: TOK_LET TOK_LNAME TOK_EQUALS regexp		{ Alias ($2, $4) }


regexps
	: or_regexps					{ match $1 with [a] -> a | l -> OrGrouping (List.rev l) }


or_regexps
	: TOK_PIPE? regexp				{ [$2] }
	| or_regexps TOK_PIPE regexp			{ $3 :: $1 }


regexp
	: full_regexp+					{ match $1 with [a] -> a | l -> AndGrouping (List.rev l) }


full_regexp
	: atom quantifier binding			{ $3 ($2 $1) }


quantifier
	: /* empty */					{ identity }
	| TOK_QUESTION					{ fun r -> Question r }
	| TOK_STAR					{ fun r -> Star r }
	| TOK_PLUS					{ fun r -> Plus r }
	| TOK_LBRACE int? TOK_COMMA int? TOK_RBRACE	{ fun r -> Quantified (r, $2, $4) }


int
	: TOK_INTEGER					{ $1 }


binding
	: /* empty */					{ identity }
	| TOK_AS TOK_LNAME				{ fun r -> Binding (r, $2) }


atom
	: TOK_STRING					{ String $1 }
	| TOK_CHAR					{ Char $1 }
	| TOK_LNAME					{ Lexeme $1 }
	| TOK_UNDERLINE					{ AnyChar }
	| TOK_LBRACK inverted char_class+ TOK_RBRACK	{ CharClass ($2 $3) }
	| TOK_LPAREN regexps TOK_RPAREN			{ $2 }


inverted
	: /* empty */					{ fun c -> Positive c }
	| TOK_CARET					{ fun c -> Negative c }


char_class
	: TOK_CHAR					{ Single $1 }
	| TOK_CHAR TOK_MINUS TOK_CHAR			{ Range ($1, $3) }


lexers
	: first_lexer and_lexer*			{ $1 :: $2 }


first_lexer
	: TOK_RULE lexer				{ $2 }


and_lexer
	: TOK_AND lexer					{ $2 }


lexer
	: lexer_head rule+				{ $1 $2 }


lexer_head
	: TOK_LNAME TOK_LNAME* TOK_EQUALS TOK_PARSE	{ fun r -> Lexer ($1, $2, r) }


rule
	: regexps code					{ Rule ($1, $2) }
