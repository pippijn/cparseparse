open Camlp4.PreCast
open GrammarType
open CodegenHelpers

let (|>) = BatPervasives.(|>)
let _loc = Loc.ghost


(************************************************
 * :: Tokens
 ************************************************)


let make_ml_token_type terms =
  TermArray.map (fun term ->
    let semtype =
      assert (is_uid term.tbase.name);
      <:ctyp<$uid:term.tbase.name$>>
    in

    match Semantic.semtype_of_term SemanticVariant.User term with
    | None    -> semtype
    | Some ty -> <:ctyp<$semtype$ of $ty$>>
  ) terms
  |> TermArray.to_list
  |> Ast.tyOr_of_list



let make_ml_token_fn ?default value terms =
  let cases =
    TermArray.fold_left (fun cases term ->
      try
        let name = term.tbase.name in
        assert (is_uid name);

        let patt =
          match Semantic.semtype_of_term SemanticVariant.User term with
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
      include Glr.TokenInfo.S with type t := t
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
      <:expr<$int:Ids.Terminal.to_string term.tbase.index_id$>>
    ) terms
  in

  let sval_fn =
    make_ml_token_fn (fun term ->
      match Semantic.semtype_of_term SemanticVariant.User term with
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
