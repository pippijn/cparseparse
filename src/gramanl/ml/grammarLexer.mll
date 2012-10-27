{
  open GrammarParser

  let (|>) = BatPervasives.(|>)


  type automaton =
    | Normal
    | Verbatim
    | Typename

  type state = {
    code : Buffer.t;
    mutable stack : Lexing.lexbuf list;
    mutable automaton : automaton;
    mutable brace_level : int;
    mutable in_rhs : bool;
  }

  let default_state lexbuf = {
    code = Buffer.create 16;
    stack = [lexbuf];
    automaton = Normal;
    brace_level = 0;
    in_rhs = false;
  }

  let keywords = [
    "delete",		TOK_DELETE;
    "forbid_next",	TOK_FORBID_NEXT;
    "option",		TOK_OPTION;
    "precedence",	TOK_PRECEDENCE;
    "replace",		TOK_REPLACE;
    "subsets",		TOK_SUBSETS;
    "terminals",	TOK_TERMINALS;
  ]

  let classify id =
    try
      snd (List.find (fun (name, token) -> Sloc.value id = name) keywords)
    with Not_found ->
      TOK_LNAME id

  let to_string = function
    | TOK_INTEGER i -> Printf.sprintf "TOK_INTEGER %d" i
    | TOK_UNAME id -> "TOK_UNAME " ^ Sloc.value id
    | TOK_LNAME id -> "TOK_LNAME " ^ Sloc.value id
    | TOK_STRING id -> "TOK_STRING " ^ Sloc.value id
    | TOK_LIT_CODE id -> "TOK_LIT_CODE " ^ Sloc.value id

    | TOK_LBRACK -> "TOK_LBRACK"
    | TOK_RBRACK -> "TOK_RBRACK"
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
    | TOK_SUBSETS -> "TOK_SUBSETS"
    | TOK_DELETE -> "TOK_DELETE"
    | TOK_REPLACE -> "TOK_REPLACE"
    | TOK_FORBID_NEXT -> "TOK_FORBID_NEXT"

    | EOF -> "EOF"


  let include_file inc =
    let startpos = String. index inc '"' + 1 in
    let endpos	 = String.rindex inc '"' in
    String.sub inc startpos (endpos - startpos)


  let remove_quotes str =
    String.sub str 1 (String.length str - 2)


  let remove_braces str =
    let startpos = String. index str '{' + 1 in
    let endpos	 = String.rindex str '}' in
    String.sub str startpos (endpos - startpos)
    |> BatString.trim


  let remove_parens str =
    let startpos = String. index str '(' + 1 in
    let endpos	 = String.rindex str ')' in
    String.sub str startpos (endpos - startpos)
    |> BatString.trim


  let output_position out p =
    let open Lexing in
    Printf.fprintf out "(%s, %d, %d)"
      p.pos_fname
      p.pos_lnum
      (p.pos_cnum - p.pos_bol)

  let loc lexbuf t =
    let s = Lexing.lexeme_start_p lexbuf in
    let e = Lexing.lexeme_end_p lexbuf in
    if Options._trace_lexing () then
      Printf.printf "%a-%a: %s\n"
	output_position s
	output_position e
	t;
    (t, s, e)

  let return lexbuf t =
    let s = Lexing.lexeme_start_p lexbuf in
    let e = Lexing.lexeme_end_p lexbuf in
    if Options._trace_lexing () then
      Printf.printf "%a-%a: %s\n"
	output_position s
	output_position e
	(to_string t);
    (t, s, e)
}


let ws = [' ' '\t' '\r']


rule verbatim state = parse
| '{'			{
			  state.brace_level <- state.brace_level + 1;
			  Buffer.add_char state.code '{';
			  verbatim state lexbuf
			}

| '}'			{
			  state.brace_level <- state.brace_level - 1;
			  Buffer.add_char state.code '}';
			  if state.brace_level = 0 then
			    let code = String.copy (Buffer.contents state.code) in
			    Buffer.clear state.code;
			    state.automaton <- Normal;
			    state.in_rhs <- false;
			    return lexbuf (TOK_LIT_CODE (loc lexbuf (remove_braces code)))
			  else
			    verbatim state lexbuf
			}

| '\n' as c		{ Buffer.add_char state.code c; Lexing.new_line lexbuf; verbatim state lexbuf }
| [^'{''}''\n']+ as s	{ Buffer.add_string state.code s; verbatim state lexbuf }


and typename state = parse
| '('			{
			  state.brace_level <- state.brace_level + 1;
			  Buffer.add_char state.code '(';
			  typename state lexbuf
			}

| ')'			{
			  state.brace_level <- state.brace_level - 1;
			  Buffer.add_char state.code ')';
			  if state.brace_level = 0 then
			    let code = String.copy (Buffer.contents state.code) in
			    Buffer.clear state.code;
			    state.automaton <- Normal;
			    return lexbuf (TOK_LIT_CODE (loc lexbuf (remove_parens code)))
			  else
			    typename state lexbuf
			}

| '\n' as c		{ Buffer.add_char state.code c; Lexing.new_line lexbuf; typename state lexbuf }
| [^'('')''\n']+ as s	{ Buffer.add_string state.code s; typename state lexbuf }


and normal state = parse
(* Whitespace *)
| '\n'								{ Lexing.new_line lexbuf; normal state lexbuf }
| ws+								{ normal state lexbuf }

(* Comments *)
| "(*"								{ comment 0 state lexbuf }

(* State-switching keywords *)
| "impl_verbatim"						{ state.automaton <- Verbatim; return lexbuf TOK_IMPL_VERBATIM }
| "verbatim"							{ state.automaton <- Verbatim; return lexbuf TOK_VERBATIM }
| "nonterm("							{ state.automaton <- Typename; state.brace_level <- 1; Buffer.add_char state.code '('; return lexbuf TOK_NONTERM }
| "nonterm"							{ return lexbuf TOK_NONTERM }
| "token"							{ state.automaton <- Typename; return lexbuf TOK_TOKEN }
| "fun"								{ state.in_rhs <- true; return lexbuf TOK_FUN }
| "include" ws* "(" ws* '"' ([^ '"']+ as file) '"' ws* ")" {
    let nextbuf = Lexing.from_channel (open_in file) in
    Lexing.(nextbuf.lex_curr_p <- { Lexing.dummy_pos with pos_fname = file });
    state.stack <- nextbuf :: state.stack;
    normal state nextbuf
  }

(* Handle this one specially, as it is a nonterminal *)
| "empty"							{ return lexbuf (TOK_UNAME (loc lexbuf "empty")) }

(* Identifier *)
| ['A'-'Z'] ['A'-'Z' 'a'-'z' '_' '0'-'9']* as name		{ return lexbuf (TOK_UNAME (loc lexbuf name)) }
| ['a'-'z' '_'] ['A'-'Z' 'a'-'z' '_' '0'-'9']* as name		{ return lexbuf (classify (loc lexbuf name)) }

(* Integer *)
| ['0'-'9']+ as int						{ return lexbuf (TOK_INTEGER (int_of_string int)) }

(* Integer *)
| '"' [^ '"' '\n']+ '"' as string				{ return lexbuf (TOK_STRING (string |> remove_quotes |> loc lexbuf)) }

(* Punctuators *)
| "{"								{ if state.in_rhs then (
								    state.brace_level <- 1;
								    Buffer.add_char state.code '{';
								    verbatim state lexbuf
								  ) else (
								    return lexbuf TOK_LBRACE
								  )
								}
| "}"								{ return lexbuf TOK_RBRACE }
| ":"								{ return lexbuf TOK_COLON }
| ";"								{ state.in_rhs <- false; return lexbuf TOK_SEMICOLON }
| "->"								{ state.in_rhs <- true; return lexbuf TOK_ARROW }
| "["								{ return lexbuf TOK_LBRACK }
| "]"								{ return lexbuf TOK_RBRACK }
| "("								{ return lexbuf TOK_LPAREN }
| ")"								{ return lexbuf TOK_RPAREN }
| ","								{ return lexbuf TOK_COMMA }

| eof {
    match state.stack with
    | _ :: (nextbuf :: _ as stack) ->
	state.stack <- stack;
	normal state nextbuf
    | _ :: [] ->
	return lexbuf EOF
    | [] -> failwith "impossible: empty lexer stack"
  }


and comment level state = parse
| [^ '(' '*' ')' '\n']+						{ comment level state lexbuf }
| "(*"								{ comment (level + 1) state lexbuf }
| "*)"								{ if level > 0 then comment (level - 1) state lexbuf else normal state lexbuf }
| '\n'								{ Lexing.new_line lexbuf; comment level state lexbuf }
| _								{ comment level state lexbuf }


{
  let token state =
    match state.stack with
    | lexbuf :: _ ->
	let token =
	  match state.automaton with
	  | Normal	-> normal
	  | Verbatim	-> verbatim
	  | Typename	-> typename
	in
	token state lexbuf
    | [] ->
	EOF, Lexing.dummy_pos, Lexing.dummy_pos
}
