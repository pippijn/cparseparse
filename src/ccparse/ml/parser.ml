let cc_lexer = Glr.Lexerint.({
  token = (fun () -> CcTokens.TOK_EOF, Lexing.dummy_pos, Lexing.dummy_pos);
  index = (fun (t, s, e) -> CcTokens.index t);
  sval  = (fun (t, s, e) -> CcTokens.sval t);
  sloc  = (fun (t, s, e) -> s, e);
})


exception ExitStatus of int

let handle_return = function
  | Unix.WEXITED 0 ->
      ()
  | Unix.WEXITED status ->
      Printf.printf "child exited with status %d\n" status;
  | Unix.WSIGNALED signum ->
      Printf.printf "child was killed by signal %d\n" signum;
      raise (ExitStatus 1)
  | Unix.WSTOPPED signum ->
      Printf.printf "child was stopped by signal %d\n" signum;
      raise (ExitStatus 1)


let glrparse actions tables filename lexer =
  let glr = Glr.Engine.makeGLR actions tables in

  let tree =
    try
      Some (Timing.time ~alloc:true "parsing" (Glr.Engine.glrParse glr) lexer)
    with Glr.Engine.Located ((start_p, end_p), e, extra) ->
      let open Lexing in
      (* print source position *)
      Printf.printf "\n%s:%d:%d: "
        filename
        (start_p.pos_lnum)
        (start_p.pos_cnum - start_p.pos_bol + 1);

      (* print exception info *)
      begin match e with
      | Glr.Engine.ParseError (state, token) ->
          Printf.printf "parse error near \"%s\" (state: %d, token: %d)\n"
            extra state token
      | Failure msg ->
          Printf.printf "failure in user actions: %s\n\n" msg;
          print_string extra
      | e ->
          Printf.printf "exception in user actions:\n  %s\n\n"
            (Printexc.to_string e);
          print_string extra
      end;

      None
  in

  (* print accounting statistics from glr.ml *)
  if Glr.Options._accounting () then (
    let open Glr.Engine in
    let stats = stats_of_glr glr in

    Printf.printf "stack nodes: num=%d max=%d\n"
      stats.numStackNodesAllocd
      stats.maxStackNodesAllocd;
    Printf.printf "detShift:     %d\n" stats.detShift;
    Printf.printf "detReduce:    %d\n" stats.detReduce;
    Printf.printf "nondetShift:  %d\n" stats.nondetShift;
    Printf.printf "nondetReduce: %d\n" stats.nondetReduce;
  );

  tree


let rec tokenise tokens token lexbuf =
  match token lexbuf with
  | CcTokens.TOK_EOF, _, _ as next ->
      if false then (
        Printf.printf "%d tokens\n" (List.length tokens);
        flush stdout;
      );
      List.rev (next :: tokens)
  | next ->
      tokenise (next :: tokens) token lexbuf


let lexer_from_list tokens =
  let tokens = ref tokens in
  Glr.Lexerint.({ cc_lexer with
    token = (fun () ->
      let next = List.hd !tokens in
      tokens := List.tl !tokens;
      next
    )
  })


let lexer_from_dump input =
  let tokens =
    BatStd.with_dispose ~dispose:close_in
      input_value (open_in_bin (input ^ ".tkd"))
  in
  lexer_from_list tokens, ignore


let lexer_from_lexing lexbuf =
  Glr.Lexerint.({ cc_lexer with
    token = (fun () ->
      Lexer.token lexbuf
    )
  })


let lexer_from_file input =
  let cin = Unix.open_process_in ("gcc -I /usr/include/qt4 -xc++ -E -P " ^ input) in

  let lexbuf = Lexing.from_channel cin in

  assert (not (Options._load_toks ()));
  let lexer =
    if Options._pp () then (
      let tokens =
        Timing.time ~alloc:true "lexing" (tokenise [] Lexer.token) lexbuf
      in

      if Options._dump_toks () then (
        if Options._load_toks () then
          failwith "-dump-toks and -load-toks are mutually exclusive";
        BatStd.with_dispose ~dispose:close_out
          (fun out -> output_value out tokens) (open_out_bin (input ^ ".tkd"));
      );

      lexer_from_list tokens
    ) else (
      assert (not (Options._dump_toks ()));
      lexer_from_lexing lexbuf
    )
  in

  lexer, (fun _ -> ignore (Unix.close_process_in cin))


let parse_file actions tables input =
  if Options._verbose () then
    print_endline ("%%% processing " ^ input);

  let lexer, dispose =
    if Options._load_toks () then
      lexer_from_dump input
    else
      lexer_from_file input
  in

  let lexer =
    if Options._ptree () then
      Glr.PtreeActions.make_lexer actions lexer
    else
      lexer
  in

  let tree =
    if Options._tokens () then
      None
    else
      BatStd.with_dispose ~dispose (glrparse actions tables input) lexer
  in

  tree


let dump_tree tree =
  BatStd.with_dispose ~dispose:close_out
    (fun out -> output_value out tree) (open_out_bin "result.bin")


let parse_files actions tables files =
  let result = Timing.alloc "parsing all files" (List.rev_map (parse_file actions tables)) files in

  if Options._sizeof_tree () then (
    let size =
      Timing.time "computing memory size of result"
        Valgrind.sizeof result
    in

    Printf.printf "=> the returned data structure is %a\n"
      Timing.output_memsize (float_of_int size);
    flush stdout;
  );

  if Options._dump_tree () then
    Timing.time "serialising tree" dump_tree result;

  result


let print_tptree tree =
  Sexplib.Sexp.output_hum stdout (CcPtree.Ptree.sexp_of_t tree);
  print_newline ()


let print_treematch tree =
  Sexplib.Sexp.output_hum stdout (CcTreematch.Ptree.sexp_of_t tree);
  print_newline ()


let elkmain inputs =
  let actions = CcActions.userActions in
  let tables  = CcTables.parseTables in

  if Options._ptree () then (

    let actions = Glr.PtreeActions.make_actions actions tables in
    let trees = parse_files actions tables inputs in
    List.iter (function
      | None -> ()
      | Some tree ->
          if Options._print () then
            Glr.PtreeNode.print_tree tree stdout true
    ) trees

  ) else if Options._tptree () then (

    let actions = CcPtreeActions.userActions in
    let trees = parse_files actions tables inputs in
    if Options._print () then
      List.iter (function
        | None -> ()
        | Some lst ->
            (* Print our parse tree *)
            print_tptree lst
      ) trees

  ) else if Options._treematch () then (

    let actions = CcTreematchActions.userActions in
    let trees = parse_files actions tables inputs in
    if Options._print () then
      List.iter (function
        | None -> ()
        | Some lst ->
            (* Print our parse tree *)
            print_treematch lst
      ) trees

  ) else if Options._trivial () then (

    let actions = Glr.UserActions.make_trivial actions in
    let trees = parse_files actions tables inputs in
    List.iter (function None | Some () -> ()) trees

  ) else (

    let trees = parse_files actions tables inputs in
    List.iter (function
      | None -> ()
      | Some tu ->
          if Options._print () then (
            Sexplib.Sexp.output_hum stdout (Ccabs.Ast.sexp_of_translation_unit tu);
            print_newline ()
          )
    ) trees

  )


let main inputs =
  if Options._rt () then
    Sched.(setscheduler 0 FIFO { priority = 20 });

  try
    elkmain inputs
  with ExitStatus status ->
    exit status