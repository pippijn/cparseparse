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

  let nfas = Timing.progress "constructing NFAs" Nfa.construct program in
  let dfas = List.map Dfa.of_nfa nfas in

  if true then (
    List.iter2 (fun (nname, nargs, nfa) (dname, dargs, dfa) ->
      assert (nname == dname);
      assert (nargs == dargs);
      Printf.printf "NFA %a:\n%a\nDFA =>\n%a\n"
        Sloc.print_string nname
        Sexplib.Sexp.output_hum (Nfa.Fsm.sexp_of_t nfa)
        Sexplib.Sexp.output_hum (Dfa.Fsm.sexp_of_t dfa)
    ) nfas dfas
  );

  ()


let parse file =
  BatStd.with_dispose ~dispose:close_in (parse_channel file) (open_in file)


let main files =
  List.iter parse files


let () =
  Cmdline.run main
