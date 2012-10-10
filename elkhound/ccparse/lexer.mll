(* lexer.mll *)
(* simple lexer for C++ *)

{
  open CcTokens
  open Cc_keywords

  let line = ref 1
}


let lower    = ['a'-'z']
let upper    = ['A'-'Z']

let digit    = ['0'-'9']

let alpha = (lower | upper)
let alnum = (alpha | digit)

let identifier = (alpha | '_')(alnum | '_')*

let bstring = '`'  ('\\' _ | [^ '\\' '`' ])* '`'
let dstring = '"'  ('\\' _ | [^ '\\' '"' ])* '"'
let sstring = '\'' ('\\' _ | [^ '\\' '\''])* '\''


let d = digit
let o = ['0'-'7']
let h = ['a'-'f' 'A'-'F' '0'-'9']
let xh = ('0'['x''X'])
let b = ['0' '1']
let xb = ('0'['b''B'])
let e = (['E''e']['+''-']?d+)
let p = (['P''p']['+''-']?d+)
let fs = (['i' 'j' 'f' 'F' 'l' 'L' 'q' 'Q' 'd' 'D']+)
let is = (['i' 'j' 'u' 'l' 'U' 'L']+)

let ws = [' ' '\t' '\r']

let u = ['\x80'-'\xbf']


rule token = parse
(* whitespace *)
| '\n'                                                          { line := !line + 1; token lexbuf }
| [' ' '\t' '\r']+                                              { token lexbuf }

(* keywords, operators *)
| "__extension__"                                               { token lexbuf }
| "("                                                           { TOK_LPAREN }
| ")"                                                           { TOK_RPAREN }
| "["                                                           { TOK_LBRACKET }
| "]"                                                           { TOK_RBRACKET }
| "->"                                                          { TOK_ARROW }
| "::"                                                          { TOK_COLONCOLON }
| "."                                                           { TOK_DOT }
| "!"                                                           { TOK_BANG }
| "~"                                                           { TOK_TILDE }
| "+"                                                           { TOK_PLUS }
| "-"                                                           { TOK_MINUS }
| "++"                                                          { TOK_PLUSPLUS }
| "--"                                                          { TOK_MINUSMINUS }
| "&"                                                           { TOK_AND }
| "*"                                                           { TOK_STAR }
| ".*"                                                          { TOK_DOTSTAR }
| "->*"                                                         { TOK_ARROWSTAR }
| "/"                                                           { TOK_SLASH }
| "%"                                                           { TOK_PERCENT }
| "<<"                                                          { TOK_LEFTSHIFT }
| ">>"                                                          { TOK_RIGHTSHIFT }
| "<"                                                           { TOK_LESSTHAN }
| "<="                                                          { TOK_LESSEQ }
| ">"                                                           { TOK_GREATERTHAN }
| ">="                                                          { TOK_GREATEREQ }
| "=="                                                          { TOK_EQUALEQUAL }
| "!="                                                          { TOK_NOTEQUAL }
| "^"                                                           { TOK_XOR }
| "|"                                                           { TOK_OR }
| "&&"                                                          { TOK_ANDAND }
| "||"                                                          { TOK_OROR }
| "?"                                                           { TOK_QUESTION }
| ":"                                                           { TOK_COLON }
| "="                                                           { TOK_EQUAL }
| "*="                                                          { TOK_STAREQUAL }
| "/="                                                          { TOK_SLASHEQUAL }
| "%="                                                          { TOK_PERCENTEQUAL }
| "+="                                                          { TOK_PLUSEQUAL }
| "-="                                                          { TOK_MINUSEQUAL }
| "&="                                                          { TOK_ANDEQUAL }
| "^="                                                          { TOK_XOREQUAL }
| "|="                                                          { TOK_OREQUAL }
| "<<="                                                         { TOK_LEFTSHIFTEQUAL }
| ">>="                                                         { TOK_RIGHTSHIFTEQUAL }
| ","                                                           { TOK_COMMA }
| "..."                                                         { TOK_ELLIPSIS }
| ";"                                                           { TOK_SEMICOLON }
| "{"                                                           { TOK_LBRACE }
| "}"                                                           { TOK_RBRACE }

(* C++ comments *)
| "//" [^ '\n']*                                                { token lexbuf }

(* C comments *)
| "/*" ([^ '*']| "*"* [^ '*' '/'])* "*"+ "/"                    { token lexbuf }
| "/*" ([^ '*']| "*"* [^ '*' '/'])* "*"*                        { failwith "unterminated comment" }

(* identifier *)
| identifier                                                    { classify (Lexing.lexeme lexbuf) }

(* integers *)
| xh h+ is?
| xb b+ is?
| '0'o+ is?
| d+    is?                                                     { TOK_INT_LITERAL (Lexing.lexeme lexbuf) }

(* floats *)
| d+e            fs?
| d*'.'d+e?      fs?
| d+'.'d*e?      fs?
| xh h*p h*      fs?
| xh h*'.'h*p h* fs?                                            { TOK_FLOAT_LITERAL (Lexing.lexeme lexbuf) }

(* strings *)
| 'L'?sstring                                                   { TOK_CHAR_LITERAL (Lexing.lexeme lexbuf) }
| 'L'?dstring                                                   { TOK_STRING_LITERAL (Lexing.lexeme lexbuf) }

| "#pragma" [^'\n']+                                            { token lexbuf }


| eof                                                           { TOK_EOF }

| _ as c                                                        { failwith (Char.escaped c) }

(* EOF *)
