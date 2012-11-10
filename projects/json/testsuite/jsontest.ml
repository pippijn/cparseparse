let slurp = false


let parse lexbuf =
  try
    ignore (Json.Parser.parse (Json.Lexer.token (Buffer.create 20)) lexbuf)
  with
  | Json.Parser.StateError (token, state) ->
      print_endline ("Error at " ^ Json.Lexer.to_string token)


let lex name lexer file =
  let lexbuf =
    if slurp then
      Timing.time "reading file" Flexing.from_file file
    else
      Lexing.from_channel (open_in file)
  in

  Valgrind.Callgrind.instrumented
    (Timing.time ("lexing with " ^ name) lexer) lexbuf


let lexer lexbuf =
  let strbuf = Buffer.create 20 in
  try
    while true do
      ignore (Json.Lexer.token strbuf lexbuf)
    done
  with End_of_file -> ()

let olexer lexbuf =
  let strbuf = Buffer.create 20 in
  try
    while true do
      ignore (Json.Olexer.token strbuf lexbuf)
    done
  with End_of_file -> ()


let yojson file =
  ignore (Timing.time "yojson" Yojson.Raw.from_file file)


let re2ml file =
  Timing.time "lexing and parsing" parse (Lexing.from_channel (open_in file));

  ignore (Sys.command "make -s -C src/json/c++");
  lex "re2ml" lexer file;
  lex "ocamllex" olexer file;
  ignore (Timing.time "lexing with flex (C++)"
    Sys.command ("src/json/c++/yylex < " ^ file));

  ignore (Timing.time "loading with JSON::XS"
    Sys.command ("src/json/perl/load " ^ file));

  yojson file;

(*
  let open Lexing in
  while not lexbuf.lex_eof_reached do
    lexbuf.lex_start_pos <- lexbuf.lex_curr_pos;
    Lexer.curr_char lexbuf 0;
    Lexer.advance lexbuf;
  done
*)
;;


(*let () = Cmdline.run (List.iter (lex "re2ml" lexer))*)
let () = Cmdline.run (List.iter re2ml)
(*let () = Cmdline.run (List.iter yojson)*)
