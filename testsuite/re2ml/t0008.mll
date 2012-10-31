let lower    = ['a'-'z']
let upper    = ['A'-'Z']

let digit    = ['0'-'9']

let alpha = (lower | upper | '$')
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
| '\n'                                                          { Lexing.new_line lexbuf; token lexbuf }
| [' ' '\t' '\r']+                                              { token lexbuf }

(* keywords, operators *)
| "__extension__"                                               { token lexbuf }
| "("                                                           { print_endline "TOK_LPAREN" }
| ")"                                                           { print_endline "TOK_RPAREN" }
| "[" | "<:"                                                    { print_endline "TOK_LBRACKET" }
| "]" | ":>"                                                    { print_endline "TOK_RBRACKET" }
| "{" | "<%"                                                    { print_endline "TOK_LBRACE" }
| "}" | "%>"                                                    { print_endline "TOK_RBRACE" }
| "->"                                                          { print_endline "TOK_ARROW" }
| "::"                                                          { print_endline "TOK_COLONCOLON" }
| "."                                                           { print_endline "TOK_DOT" }
| "!"                                                           { print_endline "TOK_BANG" }
| "~"                                                           { print_endline "TOK_TILDE" }
| "+"                                                           { print_endline "TOK_PLUS" }
| "-"                                                           { print_endline "TOK_MINUS" }
| "++"                                                          { print_endline "TOK_PLUSPLUS" }
| "--"                                                          { print_endline "TOK_MINUSMINUS" }
| "&"                                                           { print_endline "TOK_AND" }
| "*"                                                           { print_endline "TOK_STAR" }
| ".*"                                                          { print_endline "TOK_DOTSTAR" }
| "->*"                                                         { print_endline "TOK_ARROWSTAR" }
| "/"                                                           { print_endline "TOK_SLASH" }
| "%"                                                           { print_endline "TOK_PERCENT" }
| "<<"                                                          { print_endline "TOK_LEFTSHIFT" }
| ">>"                                                          { print_endline "TOK_RIGHTSHIFT" }
| "<"                                                           { print_endline "TOK_LESSTHAN" }
| "<="                                                          { print_endline "TOK_LESSEQ" }
| ">"                                                           { print_endline "TOK_GREATERTHAN" }
| ">="                                                          { print_endline "TOK_GREATEREQ" }
| "=="                                                          { print_endline "TOK_EQUALEQUAL" }
| "!="                                                          { print_endline "TOK_NOTEQUAL" }
| "^"                                                           { print_endline "TOK_XOR" }
| "|"                                                           { print_endline "TOK_OR" }
| "&&"                                                          { print_endline "TOK_ANDAND" }
| "||"                                                          { print_endline "TOK_OROR" }
| "?"                                                           { print_endline "TOK_QUESTION" }
| ":"                                                           { print_endline "TOK_COLON" }
| "="                                                           { print_endline "TOK_EQUAL" }
| "*="                                                          { print_endline "TOK_STAREQUAL" }
| "/="                                                          { print_endline "TOK_SLASHEQUAL" }
| "%="                                                          { print_endline "TOK_PERCENTEQUAL" }
| "+="                                                          { print_endline "TOK_PLUSEQUAL" }
| "-="                                                          { print_endline "TOK_MINUSEQUAL" }
| "&="                                                          { print_endline "TOK_ANDEQUAL" }
| "^="                                                          { print_endline "TOK_XOREQUAL" }
| "|="                                                          { print_endline "TOK_OREQUAL" }
| "<<="                                                         { print_endline "TOK_LEFTSHIFTEQUAL" }
| ">>="                                                         { print_endline "TOK_RIGHTSHIFTEQUAL" }
| ","                                                           { print_endline "TOK_COMMA" }
| "..."                                                         { print_endline "TOK_ELLIPSIS" }
| ";"                                                           { print_endline "TOK_SEMICOLON" }

(* GNU *)
| ">?"								{ print_endline "TOK_MAX_OP" }
| "<?"								{ print_endline "TOK_MIN_OP" }

(* C++ comments *)
| "//" [^ '\n']*                                                { token lexbuf }

(* C comments *)
| "/*" ([^ '*']| "*"* [^ '*' '/'])* "*"+ "/"                    { token lexbuf }
| "/*" ([^ '*']| "*"* [^ '*' '/'])* "*"*                        { failwith "unterminated comment" }

(* identifier *)
| identifier as id                                              { print_endline ("TOK_NAME " ^ "id") }

(* integers *)
| xh h+ is?
| xb b+ is?
| '0'o+ is?
| d+    is? as i                                                { print_endline ("TOK_INT_LITERAL " ^ "i") }

(* floats *)
| d+e            fs?
| d*'.'d+e?      fs?
| d+'.'d*e?      fs?
| xh h*p h*      fs?
| xh h*'.'h*p h* fs? as f                                       { print_endline ("TOK_FLOAT_LITERAL " ^ "f") }

(* strings *)
| 'L'?sstring as c                                              { print_endline ("TOK_CHAR_LITERAL " ^ "c") }
| 'L'?dstring as s                                              { print_endline ("TOK_STRING_LITERAL " ^ "s") }

| "#pragma" [^'\n']+                                            { token lexbuf }
