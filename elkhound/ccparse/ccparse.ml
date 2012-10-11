type ('lexbuf, 'token) lexer = {
  from_channel : in_channel -> 'lexbuf;
  lexeme : 'lexbuf -> string;
  lexeme_start : 'lexbuf -> int;

  lex : 'lexbuf -> 'token;
  line : int ref;
}


let cc_lexer = Lexerint.({
  token = (fun () -> CcTokens.TOK_EOF);
  index = CcTokens.index;
  sval  = CcTokens.sval;
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
    if Options._timing then
      Timing.time "parsing" (Glr.glrParse glr) lexer
    else
      Glr.glrParse glr lexer
  in

  (* print accounting statistics from glr.ml *)
  if Options._stats then (
    let open Glr in

    Printf.printf "stack nodes: num=%d max=%d\n"
      !Glr.numStackNodesAllocd
      !Glr.maxStackNodesAllocd;
    Printf.printf "detShift:     %d\n" glr.stats.detShift;
    Printf.printf "detReduce:    %d\n" glr.stats.detReduce;
    Printf.printf "nondetShift:  %d\n" glr.stats.nondetShift;
    Printf.printf "nondetReduce: %d\n" glr.stats.nondetReduce;
  );

  tree


let rec tokenise tokens token lexbuf =
  let next = token lexbuf in
  if next == CcTokens.TOK_EOF then (
    Printf.printf "%d tokens\n" (List.length tokens);
    flush stdout;
    List.rev (next :: tokens)
  ) else (
    tokenise (next :: tokens) token lexbuf
  )

let lexer_from_list tokens =
  let tokens = ref tokens in
  Lexerint.({ cc_lexer with
    token = (fun () ->
      let token = List.hd !tokens in
      tokens := List.tl !tokens;
      token
    )
  })


let lexer_from_dump input =
  let tokens = Marshal.from_channel (open_in_bin (input ^ ".tkd")) in
  lexer_from_list tokens


let lexer_from_lexing lexing lexbuf =
  Lexerint.({ cc_lexer with
    token = (fun () ->
      lexing.lex lexbuf
    )
  })


let lexer_from_file input lexing =
  (* FIXME: this is never closed *)
  let cin = Unix.open_process_in ("gcc -I /usr/include/qt4 -xc++ -E -P " ^ input) in

  let lexbuf = lexing.from_channel cin in

  assert (not Options._loadtoks);
  if Options._pp then (
    let tokens = tokenise [] lexing.lex lexbuf in
    if Options._dumptoks then (
      if Options._loadtoks then
        failwith "-dumptoks and -loadtoks are mutually exclusive";
      Marshal.to_channel (open_out_bin (input ^ ".tkd")) tokens [Marshal.No_sharing];
    );

    lexer_from_list tokens
  ) else (
    assert (not Options._dumptoks);
    lexer_from_lexing lexing lexbuf
  )


let parse glr actions input lexing =
  let lexer =
    if Options._loadtoks then
      lexer_from_dump input
    else
      lexer_from_file input lexing
  in

  let lexer =
    if Options._ptree then
      PtreeActions.make_lexer actions lexer
    else
      lexer
  in

  if Options._tokens then
    None
  else
    glrparse glr lexer


let parse_utf8 glr actions input =
  parse glr actions input {
    from_channel = Ulexing.from_utf8_channel;
    lexeme = Ulexing.utf8_lexeme;
    lexeme_start = Ulexing.lexeme_start;

    lex = Ulexer.token;
    line = Ulexer.line;
  }


let parse_ascii glr actions input =
  parse glr actions input {
    from_channel = Lexing.from_channel;
    lexeme = Lexing.lexeme;
    lexeme_start = Lexing.lexeme_start;

    lex = Lexer.token;
    line = Lexer.line;
  }


let parse_file glr actions input =
  if Options._utf8 then
    parse_utf8 glr actions input
  else
    parse_ascii glr actions input


let parse_files actions tables inputs =
  let glr = Glr.makeGLR actions tables in

  List.map (parse_file glr actions) inputs


let elkmain inputs =
  let actions = CcActions.userActions in
  let tables  = CcTables.parseTables in

  if Options._ptree then (

    let actions = PtreeActions.make_actions actions tables in
    let trees = parse_files actions tables inputs in
    List.iter (function
      | None -> ()
      | Some tree ->
          if Options._print then
            PtreeNode.print_tree tree stdout true
    ) trees

  ) else if Options._trivial then (

    let actions = UserActions.make_trivial actions in
    List.iter (function None | Some () -> ()) (parse_files actions tables inputs)

  ) else (

    (* unit list list *)
    List.iter (function None -> () | Some l -> List.iter (fun () -> ()) l) (parse_files actions tables inputs)

  )


let main inputs =
  try
    elkmain inputs
  with ExitStatus status ->
    exit status


let () =
  Gc.set {
    (Gc.get ()) with
    Gc.minor_heap_size = 8 * 1024 * 1024;
  };

  Printexc.record_backtrace true;
  Printexc.print main Options.inputs
