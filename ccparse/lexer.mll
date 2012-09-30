(* lexer.mll *)
(* simple lexer for C++ *)

{
  open Cc_tokens
  module Stringmap = Map.Make (String)
  exception Eof

  let keywords = List.fold_left (fun map (kw, tok) -> Stringmap.add kw tok map) Stringmap.empty [
    "asm",                 TOK_ASM;
    "__asm",               TOK_ASM;
    "__asm__",             TOK_ASM;
    "auto",                TOK_AUTO;
    "break",               TOK_BREAK;
    "bool",                TOK_BOOL;
    "case",                TOK_CASE;
    "catch",               TOK_CATCH;
    "cdecl",               TOK_CDECL;
    "char",                TOK_CHAR;
    "class",               TOK_CLASS;
    "__restrict",          TOK_RESTRICT;
    "__restrict__",        TOK_RESTRICT;
    "__const",             TOK_CONST;
    "const",               TOK_CONST;
    "const_cast",          TOK_CONST_CAST;
    "continue",            TOK_CONTINUE;
    "default",             TOK_DEFAULT;
    "delete",              TOK_DELETE;
    "do",                  TOK_DO;
    "double",              TOK_DOUBLE;
    "dynamic_cast",        TOK_DYNAMIC_CAST;
    "else",                TOK_ELSE;
    "enum",                TOK_ENUM;
    "explicit",            TOK_EXPLICIT;
    "export",              TOK_EXPORT;
    "extern",              TOK_EXTERN;
    "false",               TOK_FALSE;
    "float",               TOK_FLOAT;
    "for",                 TOK_FOR;
    "friend",              TOK_FRIEND;
    "goto",                TOK_GOTO;
    "if",                  TOK_IF;
    "inline",              TOK_INLINE;
    "int",                 TOK_INT;
    "long",                TOK_LONG;
    "mutable",             TOK_MUTABLE;
    "namespace",           TOK_NAMESPACE;
    "new",                 TOK_NEW;
    "operator",            TOK_OPERATOR;
    "pascal",              TOK_PASCAL;
    "private",             TOK_PRIVATE;
    "protected",           TOK_PROTECTED;
    "public",              TOK_PUBLIC;
    "register",            TOK_REGISTER;
    "reinterpret_cast",    TOK_REINTERPRET_CAST;
    "return",              TOK_RETURN;
    "short",               TOK_SHORT;
    "signed",              TOK_SIGNED;
    "sizeof",              TOK_SIZEOF;
    "static",              TOK_STATIC;
    "static_cast",         TOK_STATIC_CAST;
    "struct",              TOK_STRUCT;
    "switch",              TOK_SWITCH;
    "template",            TOK_TEMPLATE;
    "this",                TOK_THIS;
    "throw",               TOK_THROW;
    "true",                TOK_TRUE;
    "try",                 TOK_TRY;
    "typedef",             TOK_TYPEDEF;
    "typeid",              TOK_TYPEID;
    "typename",            TOK_TYPENAME;
    "union",               TOK_UNION;
    "unsigned",            TOK_UNSIGNED;
    "using",               TOK_USING;
    "virtual",             TOK_VIRTUAL;
    "void",                TOK_VOID;
    "volatile",            TOK_VOLATILE;
    "__volatile__",        TOK_VOLATILE;
    "wchar_t",             TOK_WCHAR_T;
    "while",               TOK_WHILE;

    (* GNU *)
    "__attribute__",       TOK___ATTRIBUTE__;
    "__FUNCTION__",        TOK___FUNCTION__;
    "__label__",           TOK___LABEL__;
    "__PRETTY_FUNCTION__", TOK___PRETTY_FUNCTION__;
    "__typeof",            TOK___TYPEOF__;
    "__typeof__",          TOK___TYPEOF__;
    "__real__",            TOK_REAL;
    "__imag__",            TOK_IMAG;
    "__complex__",         TOK_COMPLEX;
    "__extension__",       TOK___EXTENSION__;

    (* C++11 *)
    "constexpr",           TOK_CONSTEXPR;
    "decltype",            TOK_DECLTYPE;
    "__decltype",          TOK_DECLTYPE;
    "__alignof",           TOK_SIZEOF;
    "__alignof__",         TOK_SIZEOF;
    "nullptr",             TOK_NULLPTR;
    "static_assert",       TOK_STATIC_ASSERT;
    "char16_t",            TOK_CHAR16_t;
    "char32_t",            TOK_CHAR32_t;
  ]

  let classify id =
    try
      Stringmap.find id keywords
    with Not_found ->
      TOK_NAME

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
| [' ' '\t']+                                                   { token lexbuf }

(* keywords, operators *)
| "__extension__"                                               { TOK___EXTENSION__; token lexbuf }
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
| ['A'-'Z' 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '_' '0'-'9']* as id    { classify id }

(* strings *)
| (xh h+ as i) (is as is)?                                      { TOK_INT_LITERAL }
| (xb b+ as i) (is as is)?                                      { TOK_INT_LITERAL }
| ('0'o+ as i) (is as is)?                                      { TOK_INT_LITERAL }
| (d+ as i) (is as is)?                                         { TOK_INT_LITERAL }

| (d+e as f) (fs as fs)?
| (d*'.'d+e? as f) (fs as fs)?
| (d+'.'d*e? as f) (fs as fs)?                                  { TOK_FLOAT_LITERAL }
| (xh h*p h* as f) (fs as fs)?
| (xh h*'.'h*p h* as f) (fs as fs)?                             { TOK_FLOAT_LITERAL }

| sstring                       as s                            { TOK_CHAR_LITERAL }
| dstring                       as s                            { TOK_STRING_LITERAL }
| 'L'sstring                    as s                            { TOK_CHAR_LITERAL }
| 'L'dstring                    as s                            { TOK_STRING_LITERAL }

| "#pragma" [^'\n']+                                            { token lexbuf }


| eof { TOK_EOF }

| _ as c                                                        { failwith (Char.escaped c) }

(* EOF *)
