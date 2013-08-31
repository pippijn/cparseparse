exception ExitStatus of int

let handle_return = function
  | Unix.WEXITED 0 ->
      ()
  | Unix.WEXITED status ->
      Printf.printf "child exited with status %d\n" status;
      raise (ExitStatus status)
  | Unix.WSIGNALED signum ->
      Printf.printf "child was killed by signal %d\n" signum;
      raise (ExitStatus 1)
  | Unix.WSTOPPED signum ->
      Printf.printf "child was stopped by signal %d\n" signum;
      raise (ExitStatus 1)


let rec tokenise tokens token lexbuf =
  match token lexbuf with
  | Ccparse.CcTokens.TOK_EOF, _, _ as next ->
      if false then (
        Printf.printf "%d tokens\n" (List.length tokens);
        flush stdout;
      );
      List.rev (next :: tokens)
  | next ->
      tokenise (next :: tokens) token lexbuf


let lexer_from_list tokens =
  let tokens = ref tokens in
  Lexerint.({ Parser.cc_lexer with
    token = (fun () ->
      let next = List.hd !tokens in
      tokens := List.tl !tokens;
      next
    )
  })


let lexer_from_dump input =
  let tokens =
    BatPervasives.with_dispose ~dispose:close_in
      input_value (open_in_bin (input ^ ".tkd"))
  in
  lexer_from_list tokens, ignore


let lexer_from_lexing lexbuf =
  Lexerint.({ Parser.cc_lexer with
    token = (fun () ->
      Ccparse.Lexer.token lexbuf
    )
  })


let lexer_from_file input =
  let cin = Unix.open_process_in ("gcc -I /usr/include/qt4 -xc++ -E -P " ^ input) in

  let lexbuf = Lexing.from_channel cin in

  assert (not (Options._load_toks ()));
  let lexer =
    if Options._pp () then (
      let tokens =
        Timing.time ~alloc:true "lexing" (tokenise [] Ccparse.Lexer.token) lexbuf
      in

      if Options._dump_toks () then (
        if Options._load_toks () then
          failwith "-dump-toks and -load-toks are mutually exclusive";
        BatPervasives.with_dispose ~dispose:close_out
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
      PtreeActions.make_lexer actions lexer
    else
      lexer
  in

  let tree =
    if Options._tokens () then
      None
    else
      BatPervasives.with_dispose ~dispose (Parser.glrparse actions tables input) lexer
  in

  tree


let dump_tree tree =
  BatPervasives.with_dispose ~dispose:close_out
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
  Sexplib.Sexp.output_hum stdout (Ccparse.CcPtree.Ptree.sexp_of_t tree);
  print_newline ()


let print_treematch tree =
  Sexplib.Sexp.output_hum stdout (Ccparse.CcTreematch.Ptree.sexp_of_t tree);
  print_newline ()


let elkmain inputs =
  let actions = Ccparse.CcActions.userActions in
  let tables  = Ccparse.CcTables.parseTables in

  if Options._ptree () then (

    let actions = PtreeActions.make_actions actions tables in
    let trees = parse_files actions tables inputs in
    List.iter (function
      | None -> ()
      | Some tree ->
          if Options._print () then
            Printf.printf "@%d %s"
              (Obj.magic tree)
              (PtreeNode.to_string tree true)
    ) trees

  ) else if Options._tptree () then (

    let actions = Ccparse.CcPtreeActions.userActions in
    let trees = parse_files actions tables inputs in
    if Options._print () then
      List.iter (function
        | None -> ()
        | Some lst ->
            (* Print our parse tree *)
            Printf.printf "@%d "
              (Obj.magic lst);
            print_tptree lst
      ) trees

  ) else if Options._treematch () then (

    let actions = Ccparse.CcTreematchActions.userActions in
    let trees = parse_files actions tables inputs in
    if Options._print () then
      List.iter (function
        | None -> ()
        | Some lst ->
            (* Print our parse tree *)
            print_treematch lst
      ) trees

  ) else if Options._trivial () then (

    let actions = UserActions.make_trivial actions in
    let trees = parse_files actions tables inputs in
    List.iter (function None | Some () -> ()) trees

  ) else (

    let trees = parse_files actions tables inputs in
    List.iter (function
      | None -> ()
      | Some tu ->
          if Options._print () then (
            Sexplib.Sexp.output_hum stdout
              (Ccabs.Ast.sexp_of_translation_unit tu);
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


let () =
  Cmdline.run main
