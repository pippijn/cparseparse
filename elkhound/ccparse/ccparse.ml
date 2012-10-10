let inputs = ref []

module Options = struct
  let _ptree = ref false
  let _print = ref false
  let _utf8 = ref false
  let _pp = ref false
  let _tokens = ref false
  let _dumptoks = ref false
  let _loadtoks = ref false
  let _trivial = ref false
  let _timing = ref false
end

let () =
  Arg.(parse (align [
    "-ptree",		Set Options._ptree,		" build parse tree";
    "-print",		Set Options._print,		" print tree";
    "-utf8",		Set Options._utf8,		" assume source file is in UTF-8 encoding";
    "-pp",		Set Options._pp,		" fully tokenise before parsing";
    "-tokens",		Set Options._tokens,		" tokenise only; do not parse";
    "-dumptoks",	Set Options._dumptoks,		" dump tokens to file (implies -pp)";
    "-loadtoks",	Set Options._loadtoks,		" load tokens from file";
    "-trivial",		Set Options._trivial,		" use trivial user actions";
    "-timing",		Set Options._timing,		" output timing details";
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


let glrparse glr getToken =
  let tree =
    if !Options._timing then
      Timing.time "parsing" (Glr.glrParse glr getToken) Lexerint.({ tokType = 0; tokSval = SemanticValue.null })
    else
      Glr.glrParse glr getToken Lexerint.({ tokType = 0; tokSval = SemanticValue.null })
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
  if next == CcTokens.TOK_EOF then (
    Printf.printf "%d tokens\n" (List.length tokens);
    flush stdout;
    List.rev (next :: tokens)
  ) else (
    tokenise (next :: tokens) token lexbuf
  )

let global_token_list = ref []
let getToken_from_global_list lex =
  let open Lexerint in
  lex.tokType <- CcTokens.index (List.hd !global_token_list);
  global_token_list := List.tl !global_token_list;
  lex


let getToken_from_list tokens =
  global_token_list := tokens;
  getToken_from_global_list


let getToken_from_lexer lexer lexbuf =
  fun lex ->
    let open Lexerint in
    lex.tokType <- CcTokens.index (lexer.token lexbuf);
    lex


let getToken_from_dump input =
  let tokens = Marshal.from_channel (open_in_bin (input ^ ".tkd")) in
  getToken_from_list tokens


let getToken_from_file input lexer =
  let open Lexerint in

  (* FIXME: this is never closed *)
  let cin = Unix.open_process_in ("gcc -I /usr/include/qt4 -xc++ -E -P " ^ input) in

  let lexbuf = lexer.from_channel cin in

  assert (not !Options._loadtoks);
  if !Options._pp then (
    let tokens = tokenise [] lexer.token lexbuf in
    if !Options._dumptoks then (
      if !Options._loadtoks then
        failwith "-dumptoks and -loadtoks are mutually exclusive";
      Marshal.to_channel (open_out_bin (input ^ ".tkd")) tokens [Marshal.No_sharing];
    );

    getToken_from_list tokens
  ) else (
    assert (not !Options._dumptoks);
    getToken_from_lexer lexer lexbuf
  )


let parse glr actions input lexer =
  let getToken =
    if !Options._loadtoks then
      getToken_from_dump input
    else
      getToken_from_file input lexer
  in

  let getToken =
    if !Options._ptree then
      PtreeActions.getToken actions getToken
    else
      getToken
  in

  if !Options._tokens then
    None
  else
    glrparse glr getToken


let parse_utf8 glr actions input =
  parse glr actions input Lexerint.({
    from_channel = Ulexing.from_utf8_channel;
    lexeme = Ulexing.utf8_lexeme;
    lexeme_start = Ulexing.lexeme_start;

    token = Ulexer.token;
    line = Ulexer.line;
  })


let parse_ascii glr actions input =
  parse glr actions input Lexerint.({
    from_channel = Lexing.from_channel;
    lexeme = Lexing.lexeme;
    lexeme_start = Lexing.lexeme_start;

    token = Lexer.token;
    line = Lexer.line;
  })


let parse_file glr actions input =
  if !Options._utf8 then
    parse_utf8 glr actions input
  else
    parse_ascii glr actions input


let tokenKindDesc kind =
  CcTokens.desc (Obj.magic kind)


let parse_files actions tables inputs =
  let glr = Glr.makeGLR actions tables tokenKindDesc in

  List.map (parse_file glr actions) inputs


let elkmain inputs =
  let actions = CcActions.userActions in
  let tables  = CcTables.parseTables in

  if !Options._ptree then (

    let actions = PtreeActions.makeParseTreeActions actions tables in
    let trees = parse_files actions tables inputs in
    List.iter (function
      | None -> ()
      | Some tree ->
          if !Options._print then
            PtreeNode.print_tree tree stdout true
    ) trees

  ) else if !Options._trivial then (

    let actions = UserActions.make_trivial actions in
    List.iter (function None | Some () -> ()) (parse_files actions tables inputs)

  ) else (

    (* unit list list *)
    List.iter (function None -> () | Some l -> List.iter (fun () -> ()) l) (parse_files actions tables inputs)

  )


let main inputs =
  if !Options._dumptoks then
    Options._pp := true;

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
  Printexc.print main !inputs
