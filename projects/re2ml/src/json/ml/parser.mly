%{
%}

%token EOF

/* ===================== tokens ============================ */

/* tokens that have many lexical spellings */
%token <int>		TOK_INTEGER
%token <float>		TOK_FLOAT

%token <string>		TOK_NAME
%token <string>		TOK_STRING

/* keywords */
%token			TOK_NULL

/* punctuators */
%token TOK_LPAREN		/* "("	*/
%token TOK_RPAREN		/* ")"	*/
%token TOK_LBRACK		/* "["	*/
%token TOK_RBRACK		/* "]"	*/
%token TOK_LBRACE		/* "{"	*/
%token TOK_RBRACE		/* "}"	*/
%token TOK_COMMA		/* ","	*/
%token TOK_COLON		/* ":"	*/


/* ===================== productions ======================= */

%start <unit> parse
%%

/* The actions in this file simply build an Abstract Syntax Tree (AST)
 * for later processing. */


/* start symbol */
parse
	: json EOF					{ }


json
	: json_object					{ }
	| json_array					{ }
	| json_string					{ }
	| json_number					{ }
	| TOK_NULL					{ }


json_object
	: TOK_LBRACE object_members TOK_RBRACE		{ }


object_members
	: object_member					{ }
	| object_members object_member			{ }


object_member
	: TOK_STRING TOK_COLON json TOK_COMMA?		{ }
	| TOK_NAME TOK_COLON json TOK_COMMA?		{ }


json_array
	: TOK_LBRACK array_members TOK_RBRACK		{ }


array_members
	: array_member					{ }
	| array_members array_member			{ }


array_member
	: json TOK_COMMA?				{ }


json_string
	: TOK_STRING					{ }


json_number
	: TOK_INTEGER					{ }
	| TOK_FLOAT					{ }
