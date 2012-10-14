%{

%}

/* +=====~~~-------------------------------------------------------~~~=====+ */
/* |                                Tokens                                 | */
/* +=====~~~-------------------------------------------------------~~~=====+ */

%token TOK_LPAREN TOK_RPAREN
%token TOK_LBRACKET TOK_RBRACKET
%token TOK_LBRACE TOK_RBRACE
%token TOK_ARROW
%token TOK_BIARROW
%token TOK_COLONCOLON
%token TOK_DOT
%token TOK_BANG TOK_TILDE
%token TOK_PLUS TOK_MINUS TOK_STAR
%token TOK_EQUALEQUAL
%token TOK_QUESTION
%token TOK_COLON
%token TOK_EQUAL
%token TOK_COMMA
%token TOK_ELLIPSIS
%token TOK_SEMICOLON
%token TOK_BAR

%token TOK_AST

%token <string> TOK_INT_LITERAL
%token <string> TOK_CHAR_LITERAL
%token <string> TOK_STRING_LITERAL
%token <string> TOK_LABEL
%token <string> TOK_IDENT

%token TOK_EOF
%start<Ast.program> program
%%

program
: definitions=list(definition) TOK_EOF { definitions }

definition
: TOK_AST nm=TOK_IDENT TOK_LBRACE nds=list(ast_node) TOK_RBRACE { Ast.Ast (nm, nds) }

ast_node
: nm=TOK_LABEL option(TOK_BAR) clauses=separated_list(TOK_BAR,ast_clause) { nm, clauses }

ast_clause
: t=node_tag { t }

node_tag
: t=list(TOK_IDENT) { t }
