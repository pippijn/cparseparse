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
%token <string> TOK_LIDENT TOK_UIDENT

%token TOK_EOF

%start<Program.t> program
%%

program
: definitions=list(definition) TOK_EOF { definitions }

definition
: TOK_AST nm=TOK_UIDENT TOK_LBRACE nds=list(ast_node) TOK_RBRACE { Program.Ast (nm, nds) }

ast_node
: nm=TOK_LABEL option(TOK_BAR) node=node { nm, node }

node
: clauses=separated_list(TOK_BAR,ast_clause)  { Program.CustomNode clauses }
| nm=TOK_LIDENT { Program.NativeNode nm }

ast_clause
: t=constr { t }

constr
: c=nonempty_list(TOK_UIDENT) { List.hd c, List.tl c }

topl_tree
: nm=TOK_UIDENT tree=list(tree) { Tree.Tree (nm,tree) }
| TOK_LPAREN tree=topl_tree TOK_RPAREN { tree }

tree
: TOK_LPAREN nm=TOK_UIDENT tree=list(tree) TOK_RPAREN { Tree.Tree (nm, tree) }
| nm=TOK_UIDENT { Tree.Var nm }
