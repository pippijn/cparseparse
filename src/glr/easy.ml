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

  let parse actions tables lexer =
    let glr = GlrEngine.makeGLR actions tables in

    GlrEngine.glrParse glr lexer


  let parse token lexbuf action =
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

      match parse actions tables lexer with
      | None      ->
          print_endline "parsing failed"
      | Some tree ->
          PtreeNode.print_tree tree stdout true

    );
    
    if Config.typed_ptree then (
      
      let actions = PtreeAct.userActions in

      match parse actions tables lexer with
      | None      ->
          print_endline "parsing failed"
      | Some tree ->
          Sexplib.Sexp.output_hum stdout
            (Ptree.Ptree.sexp_of_t tree);
          print_newline ()

    );
    
    if Config.treematch then (
      
      let actions = PtreeAct.userActions in

      match parse actions tables lexer with
      | None      ->
          print_endline "parsing failed"
      | Some tree ->
          Sexplib.Sexp.output_hum stdout
            (Ptree.Ptree.sexp_of_t tree);
          print_newline ()

    );
    
    if Config.user then (

      match parse actions tables lexer with
      | None      ->
          print_endline "parsing failed"
      | Some result ->
          action result

    )

end
