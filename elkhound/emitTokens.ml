open Camlp4.PreCast
open GrammarType
open CodegenHelpers

let (|>) = BatPervasives.(|>)
let _loc = Loc.ghost


(************************************************
 * :: Tokens
 ************************************************)


let make_ml_token_type terms =
  Array.map (fun term ->
    let semtype =
      assert (is_uid term.tbase.name);
      <:ctyp<$uid:term.tbase.name$>>
    in

    match term.tbase.semtype with
    | None    -> semtype
    | Some ty -> <:ctyp<$semtype$ of $ty$>>
  ) terms
  |> Array.to_list
  |> Ast.tyOr_of_list



let make_ml_token_fn ?default value terms =
  let cases =
    Array.fold_left (fun cases term ->
      try
        let name = term.tbase.name in
        assert (is_uid name);

        let patt =
          match term.tbase.semtype with
          | None   -> <:patt<$uid:name$>>
          | Some _ -> <:patt<$uid:name$ sval>>
        in

        let case =
          <:match_case<$patt$ -> $value term$>>
        in

        <:match_case<$cases$ | $case$>>
      with Exit ->
        cases
    ) <:match_case<>> terms
  in

  let cases =
    match default with
    | None -> cases
    | Some case -> <:match_case<$cases$ | $case$>>
  in

  <:expr<function $cases$>>


let make_ml_tokens terms =
  (* emit token type declaration in both mli and ml *)
  let types = make_ml_token_type terms in
  let intf =
    <:sig_item<
      type t = $types$
      include Lexerint.TokenInfo with type t := t
    >>
  in

  (* emit the token functions *)
  let name_fn =
    make_ml_token_fn (fun term ->
      <:expr<$str:term.tbase.name$>>
    ) terms
  in

  let desc_fn =
    make_ml_token_fn (fun { alias; tbase = { name } } ->
      match alias with
      | ""    -> <:expr<$str:name$>>
      | alias -> <:expr<$str:alias$>>
    ) terms
  in

  let index_fn =
    make_ml_token_fn (fun term ->
      <:expr<$int:string_of_int term.term_index$>>
    ) terms
  in

  let sval_fn =
    make_ml_token_fn (fun term ->
      match term.tbase.semtype with
      | None   -> raise Exit
      | Some _ -> <:expr<SemanticValue.repr sval>>
    ) terms ~default:<:match_case<tok -> SemanticValue.null>>
  in

  let impl =
    <:str_item<
      type t = $types$
      let name = $name_fn$
      let desc = $desc_fn$
      let index = $index_fn$
      let sval = $sval_fn$
    >>
  in

  intf, impl