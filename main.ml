module CLexer = struct
  let tokenKindDesc kind =
    Cc_tokens_desc.token_desc (Obj.magic kind)
end


let input_file =
  if Array.length Sys.argv > 1 then
    Sys.argv.(1)
  else
    "test.ii"


let main () =
  Gc.set {
    (Gc.get ()) with
    Gc.minor_heap_size = 8 * 1024 * 1024;
  };

  let ptree = false in

  let tables = Cc.ccParseTables in
  let actions = Cc.ccUserActions in
  let lex = Lexerint.({ tokType = 0; sval = Useract.cNULL_SVAL }) in

  let lexbuf = Lexing.from_channel (open_in input_file) in

  let getToken lex = lex.Lexerint.tokType <- Obj.magic (Lexer.token lexbuf); lex in
  let getToken =
    if ptree then
      Ptreeact.getToken actions getToken
    else
      getToken
  in

  let module Glr = Glr.Make (CLexer) in

  let actions =
    if ptree then
      Ptreeact.makeParseTreeActions actions tables
    else
      actions
  in
  let glr = Glr.makeGLR tables actions in

  let treeTop =
    let tree = ref Useract.cNULL_SVAL in
    if Glr.glrParse glr getToken lex tree then
      !tree
    else
      failwith ("parsing at position " ^ (string_of_int (Lexing.lexeme_start lexbuf)) ^ ": " ^ (Lexing.lexeme lexbuf))
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


let () =
  (*Printexc.record_backtrace true;*)
  (*Printexc.print main ()*)
  main ()
