open Glr


let cc_lexer = Lexerint.({
  token = (fun () -> Lexing.dummy_pos, Lexing.dummy_pos, CcTokens.TOK_EOF);
  index = (fun (_, _, next) -> CcTokens.index next);
  sval  = (fun (_, _, next) -> CcTokens.sval next);
  sloc  = (fun (s, e, _   ) -> s, e);
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


let glrparse glr lexer =
  let tree =
    Timing.time "parsing" (GlrEngine.glrParse glr) lexer
  in

  (* print accounting statistics from glr.ml *)
  if Options._stats () then (
    let open GlrEngine in

    Printf.printf "stack nodes: num=%d max=%d\n"
      !GlrEngine.numStackNodesAllocd
      !GlrEngine.maxStackNodesAllocd;
    Printf.printf "detShift:     %d\n" glr.stats.detShift;
    Printf.printf "detReduce:    %d\n" glr.stats.detReduce;
    Printf.printf "nondetShift:  %d\n" glr.stats.nondetShift;
    Printf.printf "nondetReduce: %d\n" glr.stats.nondetReduce;
  );

  tree


let rec tokenise tokens token lexbuf =
  match token lexbuf with
  | _, _, CcTokens.TOK_EOF as next ->
      if false then (
        Printf.printf "%d tokens\n" (List.length tokens);
        flush stdout;
      );
      List.rev (next :: tokens)
  | next ->
      tokenise (next :: tokens) token lexbuf


let lexer_from_list tokens =
  let tokens = ref tokens in
  Lexerint.({ cc_lexer with
    token = (fun () ->
      let next = List.hd !tokens in
      tokens := List.tl !tokens;
      next
    )
  })


let lexer_from_dump input =
  let tokens = Marshal.from_channel (open_in_bin (input ^ ".tkd")) in
  lexer_from_list tokens


let lexer_from_lexing lexbuf =
  Lexerint.({ cc_lexer with
    token = (fun () ->
      Lexer.token lexbuf
    )
  })


let lexer_from_file input =
  (* FIXME: this is never closed *)
  let cin = Unix.open_process_in ("gcc -I /usr/include/qt4 -xc++ -E -P " ^ input) in

  let lexbuf = Lexing.from_channel cin in

  assert (not (Options._loadtoks ()));
  if Options._pp () then (
    let tokens = Timing.time "lexing" (tokenise [] Lexer.token) lexbuf in
    if Options._dumptoks () then (
      if Options._loadtoks () then
        failwith "-dumptoks and -loadtoks are mutually exclusive";
      Marshal.to_channel (open_out_bin (input ^ ".tkd")) tokens [Marshal.No_sharing];
    );

    lexer_from_list tokens
  ) else (
    assert (not (Options._dumptoks ()));
    lexer_from_lexing lexbuf
  )


let parse_file glr actions input =
  let lexer =
    if Options._loadtoks () then
      lexer_from_dump input
    else
      lexer_from_file input
  in

  let lexer =
    if Options._ptree () then
      PtreeActions.make_lexer actions lexer
    else
      lexer
  in

  if Options._tokens () then
    None
  else
    match glrparse glr lexer with
    | Some _ as result -> result
    | None ->
        Printf.printf "near line %d\n" 0;
        None


let parse_files actions tables inputs =
  let glr = GlrEngine.makeGLR actions tables in

  List.map (parse_file glr actions) inputs


let print_tptree tree =
  Sexplib.Sexp.output_hum stdout (CcPtree.sexp_of_t tree);
  print_newline ()


let elkmain inputs =
  let actions = CcActions.userActions in
  let tables  = CcTables.parseTables in

  if Options._ptree () then (

    let actions = PtreeActions.make_actions actions tables in
    let trees = parse_files actions tables inputs in
    List.iter (function
      | None -> ()
      | Some tree ->
          if Options._print () then
            PtreeNode.print_tree tree stdout true
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

  ) else if Options._trivial () then (

    let actions = UserActions.make_trivial actions in
    let trees = parse_files actions tables inputs in
    List.iter (function None | Some () -> ()) trees

  ) else (

    let trees = parse_files actions tables inputs in
    List.iter (function
      | None -> ()
      | Some lst -> ()
    ) trees

  )


let main inputs =
  if Options._rt () then
    Sched.(setscheduler 0 FIFO { priority = 20 });

  try
    elkmain inputs
  with ExitStatus status ->
    exit status
