(*+ -auto-loc
 *)
{
  open Parser

  let parse_number str =
    if String.length str < 19 then
      TOK_INTEGER (int_of_string str)
    else
      TOK_FLOAT (float_of_string str)

  let finish_string strbuf =
    let t = TOK_STRING (Buffer.contents strbuf) in
    Buffer.clear strbuf;
    t

  let add_lexeme strbuf lexbuf =
    let open Lexing in
    let len = lexbuf.lex_curr_pos - lexbuf.lex_start_pos in
    Buffer.add_substring strbuf lexbuf.lex_buffer lexbuf.lex_start_pos len

}


let d = ['0'-'9']


rule sstring strbuf = parse
| '\''					{ finish_string strbuf }
| "\\'"					{ Buffer.add_char strbuf '\''; sstring strbuf lexbuf }
| "\\\\"				{ Buffer.add_char strbuf '\\'; sstring strbuf lexbuf }
| "\\U" d d d d d d d d			{ Buffer.add_char strbuf 'X'; sstring strbuf lexbuf }
| "\\u" d d d d				{ Buffer.add_char strbuf 'X'; sstring strbuf lexbuf }
| "\\x" d d				{ Buffer.add_char strbuf 'X'; sstring strbuf lexbuf }
| "\\v"					{ Buffer.add_char strbuf '\x0c'; sstring strbuf lexbuf }
| "\\f"					{ Buffer.add_char strbuf '\x0b'; sstring strbuf lexbuf }
| "\\t"					{ Buffer.add_char strbuf '\t'; sstring strbuf lexbuf }
| "\\r"					{ Buffer.add_char strbuf '\r'; sstring strbuf lexbuf }
| "\\n"					{ Buffer.add_char strbuf '\n'; sstring strbuf lexbuf }
| [^ '\\' '\'']+ as s			{ Buffer.add_string strbuf s; sstring strbuf lexbuf }


and dstring strbuf = parse
| '"'					{ finish_string strbuf }
| "\\\""				{ Buffer.add_char strbuf '\''; dstring strbuf lexbuf }
| "\\\\"				{ Buffer.add_char strbuf '\\'; dstring strbuf lexbuf }
| "\\U" d d d d d d d d			{ Buffer.add_char strbuf 'X'; dstring strbuf lexbuf }
| "\\u" d d d d				{ Buffer.add_char strbuf 'X'; dstring strbuf lexbuf }
| "\\x" d d				{ Buffer.add_char strbuf 'X'; dstring strbuf lexbuf }
| "\\v"					{ Buffer.add_char strbuf '\x0c'; dstring strbuf lexbuf }
| "\\f"					{ Buffer.add_char strbuf '\x0b'; dstring strbuf lexbuf }
| "\\t"					{ Buffer.add_char strbuf '\t'; dstring strbuf lexbuf }
| "\\r"					{ Buffer.add_char strbuf '\r'; dstring strbuf lexbuf }
| "\\n"					{ Buffer.add_char strbuf '\n'; dstring strbuf lexbuf }
| [^ '\\' '"']+				{ add_lexeme strbuf lexbuf; dstring strbuf lexbuf }


and token strbuf = parse
(* Whitespace *)
| [' ' '\t' '\r' '\n']+			{ token strbuf lexbuf }

(* Keywords *)
| "null"				{ TOK_NULL }
| "true"				{ TOK_TRUE }
| "false"				{ TOK_FALSE }

(* Integer *)
| d+ as i				{ parse_number i }

(* Float *)
| (d+ '.' d+) as f			{ TOK_FLOAT (float_of_string f) }

(* String/character *)
| '"'					{ dstring strbuf lexbuf }
| '\''					{ sstring strbuf lexbuf }

(* Punctuators *)
| '{'					{ TOK_LBRACE }
| '}'					{ TOK_RBRACE }
| '['					{ TOK_LBRACK }
| ']'					{ TOK_RBRACK }
| ':'					{ TOK_COLON }
| ','					{ TOK_COMMA }

| _ as c				{ failwith (Char.escaped c) }

| eof					{ raise End_of_file }


{
  let to_string = function
    | TOK_INTEGER i -> Printf.sprintf "TOK_INTEGER %d" i
    | TOK_FLOAT f -> Printf.sprintf "TOK_FLOAT %f" f
    | TOK_STRING id -> "TOK_STRING " ^ id
    | TOK_NULL -> "TOK_NULL"
    | TOK_TRUE -> "TOK_TRUE"
    | TOK_FALSE -> "TOK_FALSE"

    | TOK_COMMA -> "TOK_COMMA"
    | TOK_COLON -> "TOK_COLON"

    | TOK_LBRACE -> "TOK_LBRACE"
    | TOK_RBRACE -> "TOK_RBRACE"
    | TOK_LBRACK -> "TOK_LBRACK"
    | TOK_RBRACK -> "TOK_RBRACK"
}
