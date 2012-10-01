%{
  open Gramast
%}

%token EOF

/* ===================== tokens ============================ */
/* tokens that have many lexical spellings */
%token <int> TOK_INTEGER
%token <string> TOK_NAME
%token <string> TOK_STRING
%token <string> TOK_LIT_CODE

/* punctuators */
%token TOK_LBRACE		/* "{"	*/
%token TOK_RBRACE		/* "}"	*/
%token TOK_COLON		/* ":"	*/
%token TOK_SEMICOLON		/* ";"	*/
%token TOK_ARROW		/* "->" */
%token TOK_LPAREN		/* "("	*/
%token TOK_RPAREN		/* ")"	*/
%token TOK_COMMA		/* ","	*/

/* keywords */
%token TOK_FUN			/* "fun"		*/
%token TOK_TERMINALS		/* "terminals"		*/
%token TOK_TOKEN		/* "token"		*/
%token TOK_NONTERM		/* "nonterm"		*/
%token TOK_VERBATIM		/* "verbatim"		*/
%token TOK_IMPL_VERBATIM	/* "impl_verbatim"	*/
%token TOK_PRECEDENCE		/* "precedence"		*/
%token TOK_OPTION		/* "option"		*/
%token TOK_CONTEXT_CLASS	/* "context_class"	*/
%token TOK_SUBSETS		/* "subsets"		*/
%token TOK_DELETE		/* "delete"		*/
%token TOK_REPLACE		/* "replace"		*/
%token TOK_FORBID_NEXT		/* "forbid_next"	*/


/* ===================== productions ======================= */

%start <Gramast.topform list> parse
%%

/* The actions in this file simply build an Abstract Syntax Tree (AST)
 * for later processing. */


/* start symbol */
parse
	: top_form_list EOF		{ List.rev $1 }


top_form_list
	: /*empty*/			{ [] }
	| top_form_list top_form	{ $2 :: $1 }
	| top_form_list context_class	{ $1 }


top_form
	: verbatim			{ $1 }
	| parser_option			{ $1 }
	| terminals			{ $1 }
	| nonterminal			{ $1 }


context_class
	: TOK_CONTEXT_CLASS TOK_LIT_CODE TOK_SEMICOLON
		{ () }


verbatim
	: TOK_VERBATIM TOK_LIT_CODE	     { TF_verbatim (false, $2) }
	| TOK_IMPL_VERBATIM TOK_LIT_CODE     { TF_verbatim (true , $2) }


/* options without specified values default to a value of 1 */
parser_option
	: TOK_OPTION TOK_NAME		  TOK_SEMICOLON { TF_option ($2,  1) }
	| TOK_OPTION TOK_NAME TOK_INTEGER TOK_SEMICOLON { TF_option ($2, $3) }
	| TOK_OPTION TOK_NAME TOK_NAME    TOK_SEMICOLON { TF_option ($2, (if $3 = "true" then 1 else 0)) }


/* ------ terminals ------ */
/*
 * the terminals are the grammar symbols that appear only on the RHS of
 * forms; they are the output of the lexer; the terminals list declares
 * all of the terminals that will appear in the rules
 */
terminals
	: TOK_TERMINALS TOK_LBRACE terminal_decls term_types precedence TOK_RBRACE
		{ TF_terminals (List.rev $3, List.rev $4, $5) }


terminal_decls
	: /* empty */				{ [] }
	| terminal_decls terminal_decl		{ $2 :: $1 }


/* each terminal has an integer code which is the integer value the
 * lexer uses to represent that terminal.  it is followed by a
 * canonical name, and an optional alias; the name/alias appears in
 * the forms, rather than the integer code itself */
terminal_decl
	: TOK_INTEGER TOK_COLON TOK_NAME	    TOK_SEMICOLON
		{ TermDecl ($1, $3, "") }
	| TOK_INTEGER TOK_COLON TOK_NAME TOK_STRING TOK_SEMICOLON
		{ TermDecl ($1, $3, $4) }


type_decl
	: TOK_LIT_CODE			{ $1 }
	| /* empty */			{ "" }


term_types
	: /* empty */			{ [] }
	| term_types term_type		{ $2 :: $1 }


term_type
	: TOK_TOKEN type_decl TOK_NAME TOK_SEMICOLON
		{ TermType ($3, $2, []) }
	| TOK_TOKEN type_decl TOK_NAME TOK_LBRACE spec_funcs TOK_RBRACE
		{ TermType ($3, $2, List.rev $5) }


precedence
	: /* empty */						{ [] }
	| TOK_PRECEDENCE TOK_LBRACE prec_specs TOK_RBRACE	{ List.rev $3 }


prec_specs
	: /* empty */
		{ [] }
	| prec_specs TOK_NAME TOK_INTEGER name_or_string_list TOK_SEMICOLON
		{ PrecSpec (Assoc.of_string $2, $3, List.rev $4) :: $1 }


name_or_string_list
	: /* empty */				{ [] }
	| name_or_string_list name_or_string	{ $2 :: $1 }


name_or_string
	: TOK_NAME		{ $1 }
	| TOK_STRING		{ $1 }



/* ------ specification functions ------ */
spec_funcs
	: /* empty */		      { [] }
	| spec_funcs spec_func	      { $2 :: $1 }


spec_func
	: TOK_FUN TOK_NAME TOK_LPAREN formals_opt TOK_RPAREN TOK_LIT_CODE
		{ SpecFunc ($2, $4, $6) }


formals_opt
	: /* empty */		      { [] }
	| formals		      { List.rev $1 }


formals
	: TOK_NAME			{ [$1] }
	| formals TOK_COMMA TOK_NAME	{ $3 :: $1 }



/* ------ nonterminals ------ */
/*
 * a nonterminal is a grammar symbol that appears on the LHS of forms;
 * the body of the nonterminal declaration specifies the the RHS forms,
 * attribute info, etc.
 */
nonterminal
	: TOK_NONTERM type_decl TOK_NAME production
		{ TF_nonterm ($3, $2, [], [$4], []) }
	| TOK_NONTERM type_decl TOK_NAME TOK_LBRACE spec_funcs productions subsets TOK_RBRACE
		{ TF_nonterm ($3, $2, List.rev $5, List.rev $6, $7) }


productions
	: /* empty */			{ [] }
	| productions production	{ $2 :: $1 }


production
	: TOK_ARROW rhs action		
		{ ProdDecl (PDK_NEW, List.rev $2, $3) }
	| TOK_REPLACE TOK_ARROW rhs action
		{ ProdDecl (PDK_REPLACE, List.rev $3, $4) }
	| TOK_DELETE TOK_ARROW rhs TOK_SEMICOLON
		{ ProdDecl (PDK_DELETE, List.rev $3, "") }


action
	: TOK_LIT_CODE				{ $1 }
	| TOK_SEMICOLON				{ "" }


rhs
	: /* empty */				{ [] }
	| rhs rhs_elt				{ $2 :: $1 }


/*
 * each element on the RHS of a form can have a tag, which appears before a
 * colon (':') if present; the tag is required if that symbol's attributes
 * are to be referenced anywhere in the actions or conditions for the form
 */
rhs_elt
	: TOK_NAME
		{ RH_name ("", $1) }
	| TOK_NAME TOK_COLON TOK_NAME
		{ RH_name ($1, $3) }
	| TOK_STRING
		{ RH_string ("", $1) }
	| TOK_NAME TOK_COLON TOK_STRING
		{ RH_string ($1, $3) }
	| TOK_PRECEDENCE TOK_LPAREN name_or_string TOK_RPAREN
		{ RH_prec ($3) }
	| TOK_FORBID_NEXT TOK_LPAREN name_or_string TOK_RPAREN
		{ RH_forbid ($3) }


subsets
	: /*empty*/				{ [] }
	| TOK_SUBSETS formals TOK_SEMICOLON	{ List.rev $2 }
