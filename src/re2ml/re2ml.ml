let state_count =
  List.fold_left (fun count (_, _, (dfa, actions)) ->
    count + Dfa.Fsm.cardinal dfa
  ) 0


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
  let program = Timing.progress "desugaring" Simplify.simplify program in

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

  if false then
    Printf.printf "finished dfa construction with %d states\n" (state_count dfas);

  let dfas =
    List.rev_map (fun (name, args, (dfa, actions)) ->
      name, args, (Minimisation.minimise dfa, actions)
    ) dfas
  in

  if false then
    Printf.printf "finished minimisation with %d states\n" (state_count dfas);

  Timing.progress "emitting ML code" (EmitCode.emit pre post) dfas;

  ()


let parse file =
  BatStd.with_dispose ~dispose:close_in (parse_channel file) (open_in file)


let main files =
  try
    List.iter parse files
  with Parser.StateError (token, state) ->
    let open Parser in
    let open Diagnostics in
    begin match token with
    | TOK_ERROR c ->
        error c "invalid input character: %s" (Char.escaped (Sloc.value c));

    | TOK_UNAME s ->
        error s "parse error near property name '%s'" (Sloc.value s);
    | TOK_LNAME s ->
        error s "parse error near identifier '%s'" (Sloc.value s);
    | TOK_BUILTIN s ->
        error s "parse error near built-in '%s'" (Sloc.value s);
    | TOK_STRING s ->
        error s "parse error near string \"%s\"" (Sloc.value s);

    | TOK_LIT_CODE s ->
        error s "parse error near user code";

    | TOK_CHAR c ->
        error c "parse error near character '%s'" (Char.escaped (Sloc.value c));

    | token ->
        error Sloc.empty_string "parse error near token %s" (Lexer.to_string token);
    end;

    raise Diagnostics.Exit


let () =
  try
    Cmdline.run main;
    Diagnostics.print ();
  with Diagnostics.Exit ->
    Diagnostics.print ();
    exit 1
