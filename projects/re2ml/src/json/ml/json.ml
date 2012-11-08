let slurp = true


let parse lexbuf =
  try
    Parser.parse Lexer.token lexbuf
  with
  | Parser.StateError (token, state) ->
      print_endline ("Error at " ^ Lexer.to_string token)


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
  while Lexer.token lexbuf != Parser.EOF do
    ()
  done

let olexer lexbuf =
  while Olexer.token lexbuf != Parser.EOF do
    ()
  done


let re2ml file =
  Timing.time "lexing and parsing" parse (Lexing.from_channel (open_in file));

  ignore (Sys.command "make -s -C src/json/c++");
  lex "re2ml" lexer file;
  lex "ocamllex" olexer file;
  ignore (Timing.time "lexing with flex (C++)"
    Sys.command ("src/json/c++/yylex < " ^ file));

  ignore (Timing.time "loading with JSON::XS"
    Sys.command ("src/json/perl/load " ^ file));

(*
  let open Lexing in
  while not lexbuf.lex_eof_reached do
    lexbuf.lex_start_pos <- lexbuf.lex_curr_pos;
    Lexer.curr_char lexbuf 0;
    Lexer.advance lexbuf;
  done
*)
;;


let yojson file =
  ignore (Timing.time "yojson" Yojson.Raw.from_file file)


let () = Cmdline.run (List.iter (lex "re2ml" lexer))
(*let () = Cmdline.run (List.iter re2ml)*)
(*let () = Cmdline.run (List.iter yojson)*)
