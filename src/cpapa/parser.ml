open Ccparse


let cc_lexer = Lexerint.({
  token = (fun () -> CcTokens.TOK_EOF, Lexing.dummy_pos, Lexing.dummy_pos);
  index = (fun (t, s, e) -> CcTokens.index t);
  sval  = (fun (t, s, e) -> CcTokens.sval t);
  sloc  = (fun (t, s, e) -> s, e);
})


let glrparse actions tables filename lexer =
  let glr = Glr.Engine.create actions tables in

  let tree =
    try
      Some (Timing.time ~alloc:true "parsing" (Glr.Engine.parse glr) lexer ())
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

  (* print accounting statistics from the parsing engine *)
  if Glr.Options._accounting () then (
    let open Glr.Engine in
    let stats = stats glr in

    Printf.printf "stack nodes: num=%d max=%d\n"
      stats.num_stack_nodes
      stats.max_stack_nodes;
    Printf.printf "LR shift:   %d\n" stats.det_shift;
    Printf.printf "LR reduce:  %d\n" stats.det_reduce;
    Printf.printf "GLR shift:  %d\n" stats.nondet_shift;
    Printf.printf "GLR reduce: %d\n" stats.nondet_reduce;
  );

  tree
