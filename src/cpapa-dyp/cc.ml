open Cc_parser
open Cc_lexer

let token lexbuf =
  let (token, _, _) = Cc_lexer.token lexbuf in
  token

let _ =
  let lexbuf = Lexing.from_channel stdin in
  try
    let pf = Cc_parser.main token lexbuf in
    Printf.printf "= %d\n\n" (fst (List.hd pf))
  with
  | Failure s -> Printf.printf "Failure - %s\n\n" s
  | Dyp.Syntax_error -> Printf.printf "Syntax error\n\n"
