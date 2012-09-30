open Grampar

type automaton =
  | Normal
  | Verbatim
  | Typename

type state = {
  basedir : string;
  code : Buffer.t;
  mutable stack : Ulexing.lexbuf list;
  mutable automaton : automaton;
  mutable brace_level : int;
  mutable in_rhs : bool;
}

let default_state basedir lexbuf = {
  basedir = basedir;
  code = Buffer.create 16;
  stack = [lexbuf];
  automaton = Normal;
  brace_level = 0;
  in_rhs = false;
}

let keywords = [
  "delete",	 TOK_DELETE;
  "forbid_next", TOK_FORBID_NEXT;
  "option",	 TOK_OPTION;
  "precedence",  TOK_PRECEDENCE;
  "replace",	 TOK_REPLACE;
  "subsets",	 TOK_SUBSETS;
  "terminals",	 TOK_TERMINALS;
]

let classify id =
  try
    snd (List.find (fun (name, token) -> id = name) keywords)
  with Not_found ->
    TOK_NAME id

let print = function
  | TOK_INTEGER i -> Printf.sprintf "TOK_INTEGER %d" i
  | TOK_NAME id -> "TOK_NAME " ^ id
  | TOK_STRING id -> "TOK_STRING " ^ id
  | TOK_LIT_CODE id -> "TOK_LIT_CODE " ^ id

  | TOK_LBRACE -> "TOK_LBRACE"
  | TOK_RBRACE -> "TOK_RBRACE"
  | TOK_COLON -> "TOK_COLON"
  | TOK_SEMICOLON -> "TOK_SEMICOLON"
  | TOK_ARROW -> "TOK_ARROW"
  | TOK_LPAREN -> "TOK_LPAREN"
  | TOK_RPAREN -> "TOK_RPAREN"
  | TOK_COMMA -> "TOK_COMMA"

  | TOK_FUN -> "TOK_FUN"
  | TOK_TERMINALS -> "TOK_TERMINALS"
  | TOK_TOKEN -> "TOK_TOKEN"
  | TOK_NONTERM -> "TOK_NONTERM"
  | TOK_VERBATIM -> "TOK_VERBATIM"
  | TOK_IMPL_VERBATIM -> "TOK_IMPL_VERBATIM"
  | TOK_PRECEDENCE -> "TOK_PRECEDENCE"
  | TOK_OPTION -> "TOK_OPTION"
  | TOK_CONTEXT_CLASS -> "TOK_CONTEXT_CLASS"
  | TOK_SUBSETS -> "TOK_SUBSETS"
  | TOK_DELETE -> "TOK_DELETE"
  | TOK_REPLACE -> "TOK_REPLACE"
  | TOK_FORBID_NEXT -> "TOK_FORBID_NEXT"

  | EOF -> "EOF"

let print token =
  (*print_endline (print token);*)
  token


let include_file inc =
  let startpos = String. index inc '"' + 1 in
  let endpos   = String.rindex inc '"' in
  String.sub inc startpos (endpos - startpos)


let rec verbatim state = lexer
| '{'		->
		  state.brace_level <- state.brace_level + 1;
		  Buffer.add_char state.code '{';
		  verbatim state lexbuf

| '}'		->
		  state.brace_level <- state.brace_level - 1;
		  Buffer.add_char state.code '}';
		  if state.brace_level = 0 then
		    let code = String.copy (Buffer.contents state.code) in
		    Buffer.clear state.code;
		    state.automaton <- Normal;
		    state.in_rhs <- false;
		    TOK_LIT_CODE code
		  else
		    verbatim state lexbuf

| [^ '{' '}']+	-> Buffer.add_string state.code (Ulexing.utf8_lexeme lexbuf); verbatim state lexbuf


let rec typename state = lexer
| '('		->
		  state.brace_level <- state.brace_level + 1;
		  Buffer.add_char state.code '(';
		  typename state lexbuf

| ')'		->
		  state.brace_level <- state.brace_level - 1;
		  Buffer.add_char state.code ')';
		  if state.brace_level = 0 then
		    let code = String.copy (Buffer.contents state.code) in
		    Buffer.clear state.code;
		    state.automaton <- Normal;
		    TOK_LIT_CODE code
		  else
		    typename state lexbuf

| [^ '(' ')']+	-> Buffer.add_string state.code (Ulexing.utf8_lexeme lexbuf); typename state lexbuf


let regexp ws = [' ' '\t' '\r' '\n']

let rec normal state = lexer
(* Whitespace *)
| ws+								-> normal state lexbuf

(* Comments *)
| "//" [^ '\n']*						-> normal state lexbuf
| "/*" ([^ '*']| "*"* [^ '*' '/'])* "*"+ "/"			-> normal state lexbuf
| "/*" ([^ '*']| "*"* [^ '*' '/'])* "*"*			-> failwith "unterminated comment"

(* State-switching keywords *)
| "context_class"						-> state.automaton <- Verbatim; TOK_CONTEXT_CLASS
| "impl_verbatim"						-> state.automaton <- Verbatim; TOK_IMPL_VERBATIM
| "verbatim"							-> state.automaton <- Verbatim; TOK_VERBATIM
| "nonterm"							-> state.automaton <- Typename; TOK_NONTERM
| "token"							-> state.automaton <- Typename; TOK_TOKEN
| "fun"								-> state.in_rhs <- true; TOK_FUN
| "include" ws* "(" ws* '"' ([^ '"']+) '"' ws* ")" ->
    let lexeme = Ulexing.utf8_lexeme lexbuf in
    let file = state.basedir ^ "/" ^ (include_file lexeme) in
    let nextbuf = Ulexing.from_utf8_channel (open_in file) in
    state.stack <- nextbuf :: state.stack;
    normal state nextbuf

(* Identifier *)
| ['A'-'Z' 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '_' '0'-'9']*		-> classify (Ulexing.utf8_lexeme lexbuf)

(* Integer *)
| ['0'-'9']+							-> TOK_INTEGER (int_of_string (Ulexing.utf8_lexeme lexbuf))

(* Integer *)
| '"' [^ '"']+ '"'						-> TOK_STRING (Ulexing.utf8_lexeme lexbuf)

(* Punctuators *)
| "{"								-> if state.in_rhs then (
  								     state.brace_level <- 1;
								     Buffer.add_char state.code '{';
								     verbatim state lexbuf
								   ) else (
								     TOK_LBRACE
								   )
| "}"								-> TOK_RBRACE
| ":"								-> TOK_COLON
| ";"								-> state.in_rhs <- false; TOK_SEMICOLON
| "->"								-> state.in_rhs <- true; TOK_ARROW
| "("								-> TOK_LPAREN
| ")"								-> TOK_RPAREN
| ","								-> TOK_COMMA

| eof ->
    match state.stack with
    | _ :: (nextbuf :: _ as stack) ->
	state.stack <- stack;
	normal state nextbuf
    | _ :: [] -> EOF
    | [] -> failwith "impossible: empty lexer stack"


let token state =
  match state.stack with
  | lexbuf :: _ ->
      begin match state.automaton with
      | Normal	 -> print (normal state lexbuf)
      | Verbatim -> print (verbatim state lexbuf)
      | Typename -> print (typename state lexbuf)
      end
  | [] ->
      EOF
