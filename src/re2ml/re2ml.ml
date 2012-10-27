let parse_channel file input =
  let lexbuf = Lexing.from_channel input in
  Lexing.(lexbuf.lex_curr_p <- {
    pos_fname = file;
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = 0;
  });

  let state = Lexer.make () in

  let program = Parser.parse (Lexer.token state) lexbuf in
  let program = Resolve.resolve program in

  Nfa.construct program;

  Sexplib.Sexp.output_hum stdout (Ast.sexp_of_t program);
  print_newline ()


let parse file =
  BatStd.with_dispose ~dispose:close_in (parse_channel file) (open_in file)


let main files =
  List.iter parse files


let () =
  Cmdline.run main
