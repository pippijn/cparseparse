let inputs = ref []

module Options = struct
  let _ptree = ref false
  let _print = ref false
  let _utf8 = ref false
  let _pp = ref false
  let _tokens = ref false
  let _trivial = ref false
end

let () =
  Arg.(parse (align [
    "-ptree",	Set Options._ptree,	       " build parse tree";
    "-print",	Set Options._print,	       " print tree";
    "-utf8",	Set Options._utf8,	       " assume source file is in UTF-8 encoding";
    "-pp",	Set Options._pp,	       " fully tokenise before parsing";
    "-tokens",	Set Options._tokens,	       " tokenise only; do not parse";
    "-trivial",	Set Options._trivial,	       " use trivial user actions";
  ]) (fun input -> inputs := input :: !inputs) "Usage: cxxparse [option...] <file...>")


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


let tokenKindDesc kind =
  Cc_tokens.token_desc (Obj.magic kind)


let glrparse glr getToken =
  let open Lexerint in

  let tree =
    let lex = { tokType = 0; tokSval = SemanticValue.null } in

    match Glr.glrParse glr tokenKindDesc getToken lex with
    | Some tree ->
        tree
    | None ->
        failwith "parse error"
  in

  (* print accounting statistics from glr.ml *)
  begin
    let open Glr in

    Printf.printf "stack nodes: num=%d max=%d\n"
      !Glr.numStackNodesAllocd
      !Glr.maxStackNodesAllocd;
    Printf.printf "detShift:     %d\n" glr.stats.detShift;
    Printf.printf "detReduce:    %d\n" glr.stats.detReduce;
    Printf.printf "nondetShift:  %d\n" glr.stats.nondetShift;
    Printf.printf "nondetReduce: %d\n" glr.stats.nondetReduce;
  end;

  tree


let rec tokenise tokens token lexbuf =
  let next = token lexbuf in
  if next == Cc_tokens.TOK_EOF then
    List.rev (next :: tokens)
  else
    tokenise (next :: tokens) token lexbuf


let parse glr actions cin lexer =
  let open Lexerint in

  let lexbuf = lexer.from_channel cin in

  let getToken =
    if !Options._pp then
      let tokens = ref (tokenise [] lexer.token lexbuf) in
      fun lex ->
        lex.tokType <- Cc_tokens.token_index (List.hd !tokens);
        tokens := List.tl !tokens;
        lex
    else
      fun lex ->
        lex.tokType <- Cc_tokens.token_index (lexer.token lexbuf);
        lex
  in

  let getToken =
    if !Options._ptree then
      Ptreeact.getToken actions getToken
    else
      getToken
  in

  if !Options._tokens then
    raise Exit;

  glrparse glr getToken


let parse_utf8 glr actions cin =
  let open Lexerint in
  parse glr actions cin {
    from_channel = Ulexing.from_utf8_channel;
    lexeme = Ulexing.utf8_lexeme;
    lexeme_start = Ulexing.lexeme_start;

    token = Ulexer.token;
    line = Ulexer.line;
  }


let parse_ascii glr actions cin =
  let open Lexerint in
  parse glr actions cin {
    from_channel = Lexing.from_channel;
    lexeme = Lexing.lexeme;
    lexeme_start = Lexing.lexeme_start;

    token = Lexer.token;
    line = Lexer.line;
  }


let parse_channel glr actions cin =
  if !Options._utf8 then
    parse_utf8 glr actions cin
  else
    parse_ascii glr actions cin


let parse_file glr actions input =
  let cin = Unix.open_process_in ("gcc -I /usr/include/qt4 -xc++ -E -P " ^ input) in

  let tree =
    try
      parse_channel glr actions cin
    with
    | e ->
        handle_return (Unix.close_process_in cin);
        raise e
  in
  handle_return (Unix.close_process_in cin);
  tree


let parse_files actions tables =
  let glr = Glr.makeGLR tables actions in

  List.map (parse_file glr actions) !inputs


let elkmain () =
  let actions = CcActions.userActions in
  let tables  = CcTables.parseTables in

  if !Options._ptree then (

    let actions = Ptreeact.makeParseTreeActions actions tables in
    let trees = parse_files actions tables in
    List.iter (fun tree ->
      if !Options._print then
        Ptreenode.printTree tree stdout true
    ) trees

  ) else if !Options._trivial then (

    let actions = UserActions.make_trivial actions in
    List.iter (fun () -> ()) (parse_files actions tables)

  ) else (

    (* unit list list *)
    List.iter (List.iter (fun () -> ())) (parse_files actions tables)

  )


let () =
  Gc.set {
    (Gc.get ()) with
    Gc.minor_heap_size = 8 * 1024 * 1024;
  };

  try
    Printexc.record_backtrace true;
    Printexc.print elkmain ()
  with
  | ExitStatus status ->
      exit status
