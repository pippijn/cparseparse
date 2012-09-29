module CLexer = struct
  let tokenKindDesc kind =
    Cc_tokens_desc.token_desc (Obj.magic kind)
end

let inputs = ref []

module Options = struct
  let _e = ref false
  let _ptree = ref false
end

let () =
  Arg.(parse (align [
    "-E", Set Options._e,                      " print preprocessed file";
    "-ptree",	Set Options._ptree,	       " print parse tree";
  ]) (fun input -> inputs := input :: !inputs) "Usage: cxxparse [option...] <file...>")


exception ExitStatus of int

let handle_return = function
  | Unix.WEXITED 0 ->
      ()
  | Unix.WEXITED status ->
      raise (ExitStatus status)
  | Unix.WSIGNALED signum ->
      Printf.printf "child was killed by signal %d\n" signum;
      raise (ExitStatus 1)
  | Unix.WSTOPPED signum ->
      Printf.printf "child was stopped by signal %d\n" signum;
      raise (ExitStatus 1)


let parse glr actions cin =
  let ptree = !Options._ptree in

    let lex = Lexerint.({ tokType = 0; sval = Useract.cNULL_SVAL }) in

    let lexbuf = Lexing.from_channel cin in

    let getToken lex = lex.Lexerint.tokType <- Obj.magic (Lexer.token lexbuf); lex in
    let getToken =
      if ptree then
        Ptreeact.getToken actions getToken
      else
        getToken
    in

    let treeTop =
      let tree = ref Useract.cNULL_SVAL in
      if Glr.glrParse glr CLexer.tokenKindDesc getToken lex tree then
        !tree
      else
        failwith ("parsing on line " ^ (string_of_int !Lexer.line) ^ ", position " ^ (string_of_int (Lexing.lexeme_start lexbuf)) ^ ": " ^ (Lexing.lexeme lexbuf))
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

    if ptree then
      let t = Ptreeact.project treeTop in
      Ptreenode.printTree t stdout true;
      ()


let main () =
  Gc.set {
    (Gc.get ()) with
    Gc.minor_heap_size = 8 * 1024 * 1024;
  };

  let ptree = !Options._ptree in

  let tables = Cc.ccParseTables in
  let actions = Cc.ccUserActions in

  let actions =
    if ptree then
      Ptreeact.makeParseTreeActions actions tables
    else
      actions
  in
  let glr = Glr.makeGLR tables actions in

  List.iter (fun input ->
    let cin = Unix.open_process_in (Printf.sprintf "gcc -xc++ -E -P %s" input) in

    begin try
      parse glr actions cin
    with
    | e ->
        handle_return (Unix.close_process_in cin);
        raise e
    end;
    handle_return (Unix.close_process_in cin)

  ) !inputs


let () =
  try
    (*Printexc.record_backtrace true;*)
    (*Printexc.print main ()*)
    main ()
  with
  | ExitStatus status ->
      exit status
