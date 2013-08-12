let cc_lexer = Glr.Lexerint.({
  token = (fun () -> CcTokens.TOK_EOF, Lexing.dummy_pos, Lexing.dummy_pos);
  index = (fun (t, s, e) -> CcTokens.index t);
  sval  = (fun (t, s, e) -> CcTokens.sval t);
  sloc  = (fun (t, s, e) -> s, e);
})


let glrparse actions tables filename lexer =
  let glr = Glr.Engine.create actions tables in

  let tree =
    try
      Some (Timing.time ~alloc:true "parsing" (Glr.Engine.parse glr) lexer)
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
