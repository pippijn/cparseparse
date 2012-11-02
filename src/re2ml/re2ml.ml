let parse_channel file input =
  let lexbuf = Lexing.from_channel input in
  Lexing.(lexbuf.lex_curr_p <- {
    pos_fname = file;
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = 0;
  });

  let state = Lexer.make () in

  let program = Timing.progress "parsing" (Parser.parse (Lexer.token state)) lexbuf in
  let program = Timing.progress "expanding aliases" Resolve.resolve program in

  let pre, post, nfas = Timing.progress "constructing NFAs" Nfa.construct program in
  let dfas = List.map Dfa.of_nfa nfas in

  List.iter (fun (_, _, (dfa, actions)) ->
    Reachability.check_reachable dfa actions
  ) dfas;

  if Options._dump_automata () then (
    List.iter2 (fun (nname, nargs, (nfa, nactions)) (dname, dargs, (dfa, dactions)) ->
      assert (nname == dname);
      assert (nargs == dargs);
      assert (nactions == dactions);
      Printf.printf "NFA %a:\n%a\nDFA =>\n%a\n"
        Sloc.print_string nname
        Sexplib.Sexp.output_hum (Nfa.Fsm.sexp_of_t nfa)
        Sexplib.Sexp.output_hum (Dfa.Fsm.sexp_of_t dfa)
    ) nfas dfas
  );

  let state_count =
    List.fold_left (fun count (_, _, (dfa, actions)) ->
      count + Dfa.Fsm.cardinal dfa
    ) 0 dfas
  in

  if false then
    Printf.printf "finished dfa construction with %d states\n" state_count;

  Timing.progress "emitting ML code" (EmitCode.emit pre post) dfas;

  ()


let parse file =
  BatStd.with_dispose ~dispose:close_in (parse_channel file) (open_in file)


let main files =
  List.iter parse files


let () =
  Cmdline.run main
