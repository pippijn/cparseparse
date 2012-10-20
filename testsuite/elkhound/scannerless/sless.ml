open Glr

let print_tree tree =
  Sexplib.Sexp.output_hum stdout
    (RecipePtree.sexp_of_t tree)


let print_tree tree =
  let open RecipePtree in
  match tree with
  | Start.P4 (_, p) ->
      Printf.printf "%x\n" (Obj.magic p)
  | _ ->
      print_endline "nok"


let main =
  List.iter (fun file ->
    let input = open_in file in

    let tables = RecipeTables.parseTables in
    let actions = RecipePtreeActions.userActions in

    let lexer = Lexerint.({
      token = (fun () ->
        let chr = input_char input in
        Printf.printf "> %c\n" chr;

        let open RecipeTokens in
        match chr with
        | 'a' -> A
        | 'b' -> B
        | 'c' -> C
        | 'd' -> D
        | 'e' -> E
        | 'f' -> F
        | 'g' -> G
        | 'h' -> H
        | 'i' -> I
        | 'j' -> J
        | 'k' -> K
        | 'l' -> L
        | 'm' -> M
        | 'n' -> N
        | 'o' -> O
        | 'p' -> P
        | 'q' -> Q
        | 'r' -> R
        | 's' -> S
        | 't' -> T
        | 'u' -> U
        | 'v' -> V
        | 'w' -> W
        | 'x' -> X
        | 'y' -> Y
        | 'z' -> Z
        | '\n' -> CHAR_NEWLINE
        | ' ' -> CHAR_SPACE
        | '(' -> CHAR_LPAREN
        | ')' -> CHAR_RPAREN
        | c -> failwith (Char.escaped c)
      );
      index = RecipeTokens.index;
      sval  = RecipeTokens.sval;
      sloc  = (fun _ -> Lexing.dummy_pos, Lexing.dummy_pos);
    }) in

    let actions = PtreeActions.make_actions actions tables in
    let lexer = PtreeActions.make_lexer actions lexer in

    let glr = GlrEngine.makeGLR actions tables in

    match GlrEngine.glrParse glr lexer with
    | None      ->
        print_endline "parsing failed"
    | Some tree ->
        PtreeNode.print_tree tree stdout true
  )


let () =
  Printexc.record_backtrace true;
  Cmdline.run main
