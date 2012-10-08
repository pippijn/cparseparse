let inputs = ref []

module Options = struct
  let _ptree = ref false
  let _print = ref false
  let _utf8 = ref false
  let _tokens = ref false
end

let () =
  Arg.(parse (align [
    "-ptree",	Set Options._ptree,	       " build parse tree";
    "-print",	Set Options._print,	       " print tree";
    "-utf8",	Set Options._utf8,	       " assume source file is in UTF-8 encoding";
    "-tokens",	Set Options._tokens,	       " tokenise only; do not parse";
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


let glrparse glr actions lexer getToken lexbuf =
  let open Lexerint in

  let ptree = !Options._ptree in
  let print = !Options._print in

  let getToken =
    if ptree then
      Ptreeact.getToken actions getToken
    else
      getToken
  in

  let tree =
    let lex = { tokType = 0; sval = SemanticValue.null } in

    match Glr.glrParse glr tokenKindDesc getToken lex with
    | Some tree ->
        tree
    | None ->
        let msg =
          Printf.sprintf "parsing on line %d, position %d: %s"
            !(lexer.line)
            (lexer.lexeme_start lexbuf)
            (lexer.lexeme lexbuf)
        in
        failwith msg
  in

  (* print accounting statistics from glr.ml *)
  begin
    let open Glr in

    Printf.printf "stack nodes: num=%d max=%d\n"
      !Glr.numStackNodesAllocd
      !Glr.maxStackNodesAllocd;
    Printf.printf "detShift:     %d\n" glr.detShift;
    Printf.printf "detReduce:    %d\n" glr.detReduce;
    Printf.printf "nondetShift:  %d\n" glr.nondetShift;
    Printf.printf "nondetReduce: %d\n" glr.nondetReduce;
  end;

  if ptree && print then
    let t = Ptreeact.project tree in
    Ptreenode.printTree t stdout true


let rec tokenise token lexbuf =
  if token lexbuf = Cc_tokens.TOK_EOF then
    ()
  else
    tokenise token lexbuf


let parse glr actions cin lexer =
  let open Lexerint in

  let lexbuf = lexer.from_channel cin in

  let getToken lex =
    lex.tokType <- Cc_tokens.token_index (lexer.token lexbuf);
    lex
  in

  if !Options._tokens then
    tokenise lexer.token lexbuf
  else
    glrparse glr actions lexer getToken lexbuf


let tables, actions =
  (*
  if true then
    Cc.ccParseTables, Cc.ccUserActions
  else
  *)
    CcTables.parseTables, CcActions.userActions

let elkmain () =
  let actions =
    if !Options._ptree then
      Ptreeact.makeParseTreeActions actions tables
    else
      actions
  in
  let glr = Glr.makeGLR tables actions in

  List.iter (fun input ->
    let cin = Unix.open_process_in ("gcc -I /usr/include/qt4 -xc++ -E -P " ^ input) in

    begin try
      let open Lexerint in
      if !Options._utf8 then
        parse glr actions cin {
          from_channel = Lexing.from_channel;
          lexeme = Lexing.lexeme;
          lexeme_start = Lexing.lexeme_start;

          token = Lexer.token;
          line = Lexer.line;
        }
      else
        parse glr actions cin {
          from_channel = Ulexing.from_utf8_channel;
          lexeme = Ulexing.utf8_lexeme;
          lexeme_start = Ulexing.lexeme_start;

          token = Ulexer.token;
          line = Ulexer.line;
        }
    with
    | e ->
        handle_return (Unix.close_process_in cin);
        raise e
    end;
    handle_return (Unix.close_process_in cin)

  ) !inputs


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
