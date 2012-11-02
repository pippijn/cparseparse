let regexp s =
  let lexbuf = Lexing.from_string s in
  try
    Parser.parse_regexp (Lexer.token (Lexer.make ())) lexbuf
  with Parser.StateError (token, state) ->
    Printf.printf "parse error near '%s' in state %d\n"
      (Lexer.to_string token)
      state;
    exit 1
