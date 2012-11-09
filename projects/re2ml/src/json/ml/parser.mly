%{
  open Basic
%}

/* ===================== tokens ============================ */

/* tokens that have many lexical spellings */
%token <int>		TOK_INTEGER
%token <float>		TOK_FLOAT
%token <string>		TOK_STRING

/* keywords */
%token			TOK_NULL
%token			TOK_TRUE
%token			TOK_FALSE

/* punctuators */
%token TOK_LBRACK		/* "["	*/
%token TOK_RBRACK		/* "]"	*/
%token TOK_LBRACE		/* "{"	*/
%token TOK_RBRACE		/* "}"	*/
%token TOK_COMMA		/* ","	*/
%token TOK_COLON		/* ":"	*/


/* ===================== productions ======================= */

%start <Basic.json> parse
%%

/* The actions in this file simply build an Abstract Syntax Tree (AST)
 * for later processing. */


/* start symbol */
parse:
	| json_object					{ $1 }
	| json_array					{ $1 }


json:
	| json_object					{ $1 }
	| json_array					{ $1 }
	| json_string					{ $1 }
	| json_number					{ $1 }
	| json_bool					{ $1 }
	| TOK_NULL					{ `Null }


json_object:
	| TOK_LBRACE object_members TOK_RBRACE		{ `Assoc ($2) }


object_members:
	| object_member					{ [$1] }
	| object_members TOK_COMMA object_member	{ $3 :: $1 }


object_member:
	| TOK_STRING TOK_COLON json			{ $1, $3 }


json_array:
	| TOK_LBRACK array_members TOK_RBRACK		{ `List ($2) }


array_members:
	| json						{ [$1] }
	| array_members TOK_COMMA json			{ $3 :: $1 }


json_string:
	| TOK_STRING					{ `String $1 }


json_number:
	| TOK_INTEGER					{ `Int $1 }
	| TOK_FLOAT					{ `Float $1 }


json_bool:
	| TOK_TRUE					{ `Bool true }
	| TOK_FALSE					{ `Bool false }
