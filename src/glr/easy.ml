module type Config = sig
  (* make it print a parse tree instead of evaluating the expression *)
  val ptree : bool
  (* produce a typed parse tree and print it as s-expressions *)
  val typed_ptree : bool
  (* produce and print treematch-backed parse tree *)
  val treematch : bool
  (* perform user actions *)
  val user : bool
end


module Make
  (Actions : UserActions.S)
  (Tables : ParseTablesType.S)
  (Ptree : PtreeActions.S)
  (PtreeAct : UserActions.S with type result = Ptree.Ptree.t)
  (Treematch : PtreeActions.S)
  (TreematchAct : UserActions.S with type result = Treematch.Ptree.t)
  (Tokens : TokenInfo.S)
  (Config : Config)
= struct

  let parse action filename actions tables lexer =
    let glr = GlrEngine.makeGLR actions tables in

    try
      action (GlrEngine.glrParse glr lexer)
    with GlrEngine.Located ((start_p, end_p), e) ->
      let open Lexing in
      (* print source position *)
      Printf.printf "\n%s:%d:%d: "
        filename
        (start_p.pos_lnum)
        (start_p.pos_cnum - start_p.pos_bol + 1);

      (* print exception info *)
      begin match e with
      | GlrEngine.ParseError (state, token) ->
          Printf.printf "parse error (state: %d, token: %d)"
          state token
      | Failure msg ->
          Printf.printf "failure in user actions: %s" msg
      | e ->
          Printf.printf "exception in user actions: %s"
          (Printexc.to_string e)
      end;

      (* finish printing error *)
      print_newline ()


  let parse action filename token lexbuf =
    let tables = Tables.parseTables in
    let actions = Actions.userActions in

    let lexer = Lexerint.({
      token = (fun () ->
        let token = token lexbuf in
        let start_p = Lexing.lexeme_start_p lexbuf in
        let end_p = Lexing.lexeme_end_p lexbuf in
        start_p, end_p, token
      );
      index = (fun (_, _, t) -> Tokens.index t);
      sval  = (fun (_, _, t) -> Tokens.sval  t);
      sloc  = (fun (s, e, _) -> s, e);
    }) in

    if Config.ptree then (
      let actions = PtreeActions.make_actions actions tables in
      let lexer = PtreeActions.make_lexer actions lexer in

      parse (fun tree ->
        PtreeNode.print_tree tree stdout true
      ) filename actions tables lexer
    );
    
    if Config.typed_ptree then (
      let actions = PtreeAct.userActions in

      parse (fun tree ->
        Sexplib.Sexp.output_hum stdout
          (Ptree.Ptree.sexp_of_t tree);
        print_newline ()
      ) filename actions tables lexer
    );
    
    if Config.treematch then (
      let actions = PtreeAct.userActions in

      parse (fun tree ->
        Sexplib.Sexp.output_hum stdout
          (Ptree.Ptree.sexp_of_t tree);
        print_newline ()
      ) filename actions tables lexer
    );
    
    if Config.user then (
      parse action filename actions tables lexer
    )

end
