(* lexer.mll *)
(* simple lexer for C++ *)

open CcTokens
open Cc_keywords

let line = ref 1


let regexp lower    = ['a'-'z']
let regexp upper    = ['A'-'Z']

let regexp digit    = ['0'-'9']

let regexp alpha = (lower | upper)
let regexp alnum = (alpha | digit)

let regexp identifier = (alpha | '_')(alnum | '_')*

let regexp bstring = '`'  ('\\' _ | [^ '\\' '`' ])* '`'
let regexp dstring = '"'  ('\\' _ | [^ '\\' '"' ])* '"'
let regexp sstring = '\'' ('\\' _ | [^ '\\' '\''])* '\''


let regexp d = digit
let regexp o = ['0'-'7']
let regexp h = ['a'-'f' 'A'-'F' '0'-'9']
let regexp xh = ('0'['x''X'])
let regexp b = ['0' '1']
let regexp xb = ('0'['b''B'])
let regexp e = (['E''e']['+''-']?d+)
let regexp p = (['P''p']['+''-']?d+)
let regexp fs = (['i' 'j' 'f' 'F' 'l' 'L' 'q' 'Q' 'd' 'D']+)
let regexp is = (['i' 'j' 'u' 'l' 'U' 'L']+)

let regexp ws = [' ' '\t' '\r']

let regexp u = ['\x80'-'\xbf']


let rec token = lexer
(* whitespace *)
| '\n'								-> line := !line + 1; token lexbuf
| [' ' '\t']+							-> token lexbuf

(* keywords, operators *)
| "__extension__"						-> token lexbuf
| "("								-> TOK_LPAREN
| ")"								-> TOK_RPAREN
| "["								-> TOK_LBRACKET
| "]"								-> TOK_RBRACKET
| "->"								-> TOK_ARROW
| "::"								-> TOK_COLONCOLON
| "."								-> TOK_DOT
| "!"								-> TOK_BANG
| "~"								-> TOK_TILDE
| "+"								-> TOK_PLUS
| "-"								-> TOK_MINUS
| "++"								-> TOK_PLUSPLUS
| "--"								-> TOK_MINUSMINUS
| "&"								-> TOK_AND
| "*"								-> TOK_STAR
| ".*"								-> TOK_DOTSTAR
| "->*"								-> TOK_ARROWSTAR
| "/"								-> TOK_SLASH
| "%"								-> TOK_PERCENT
| "<<"								-> TOK_LEFTSHIFT
| ">>"								-> TOK_RIGHTSHIFT
| "<"								-> TOK_LESSTHAN
| "<="								-> TOK_LESSEQ
| ">"								-> TOK_GREATERTHAN
| ">="								-> TOK_GREATEREQ
| "=="								-> TOK_EQUALEQUAL
| "!="								-> TOK_NOTEQUAL
| "^"								-> TOK_XOR
| "|"								-> TOK_OR
| "&&"								-> TOK_ANDAND
| "||"								-> TOK_OROR
| "?"								-> TOK_QUESTION
| ":"								-> TOK_COLON
| "="								-> TOK_EQUAL
| "*="								-> TOK_STAREQUAL
| "/="								-> TOK_SLASHEQUAL
| "%="								-> TOK_PERCENTEQUAL
| "+="								-> TOK_PLUSEQUAL
| "-="								-> TOK_MINUSEQUAL
| "&="								-> TOK_ANDEQUAL
| "^="								-> TOK_XOREQUAL
| "|="								-> TOK_OREQUAL
| "<<="								-> TOK_LEFTSHIFTEQUAL
| ">>="								-> TOK_RIGHTSHIFTEQUAL
| ","								-> TOK_COMMA
| "..."								-> TOK_ELLIPSIS
| ";"								-> TOK_SEMICOLON
| "{"								-> TOK_LBRACE
| "}"								-> TOK_RBRACE

(* C++ comments *)
| "//" [^ '\n']*						-> token lexbuf

(* C comments *)
| "/*" ([^ '*']| "*"* [^ '*' '/'])* "*"+ "/"			-> token lexbuf
| "/*" ([^ '*']| "*"* [^ '*' '/'])* "*"*			-> failwith "unterminated comment"

(* identifier *)
| identifier							-> classify (Ulexing.utf8_lexeme lexbuf)

(* integers *)
| xh h+ is?
| xb b+ is?
| '0'o+ is?
| d+	is?							-> TOK_INT_LITERAL (Ulexing.utf8_lexeme lexbuf)

(* floats *)
| d+e		 fs?
| d*'.'d+e?	 fs?
| d+'.'d*e?	 fs?
| xh h*p h*	 fs?
| xh h*'.'h*p h* fs?						-> TOK_FLOAT_LITERAL (Ulexing.utf8_lexeme lexbuf)

(* strings *)
| 'L'?sstring							-> TOK_CHAR_LITERAL (Ulexing.utf8_lexeme lexbuf)
| 'L'?dstring							-> TOK_STRING_LITERAL (Ulexing.utf8_lexeme lexbuf)

| "#pragma" [^'\n']+						-> token lexbuf


| eof								-> TOK_EOF

| _								-> failwith (Ulexing.utf8_lexeme lexbuf)

(* EOF *)
