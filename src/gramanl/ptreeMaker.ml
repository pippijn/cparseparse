open GrammarType
open Camlp4.PreCast

let (|>) = BatPervasives.(|>)

let _loc = Loc.ghost


(* yield all tagged symbols *)
let symbols_of_production prod =
  List.fold_right (fun sym syms ->
    match sym with
    | Terminal (tag, _)
    | Nonterminal (tag, _) when tag <> "" -> sym :: syms

    | _ -> syms
  ) prod.right []


let is_tail_of tail head_tail =
  let tail_syms = symbols_of_production tail in
  let head_tail_syms = symbols_of_production head_tail in

  match tail_syms, head_tail_syms with
  (* possibly empty list *)
  | [], [_; _] -> true
  (* non-empty list *)
  | [tail1], [_; tail2] -> tail1 = tail2
  | _ -> false


let is_left_recursion head_tail =
  match symbols_of_production head_tail with
  | [Nonterminal (_, nonterm); _] -> nonterm == head_tail.left
  | _ -> false


(* nonterminal with a single production with a tagged symbol *)
let is_singleton_nonterminal prod =
  match symbols_of_production prod with
  | [sym] -> true
  | _ -> false


(* checks whether a nonterminal produces a list of another symbol *)
let is_list_nonterminal tail head_tail =
  is_tail_of tail head_tail
  && is_left_recursion head_tail


let is_option_nonterminal none some =
  symbols_of_production none = []
  && List.length (symbols_of_production some) = 1


let is_boolean_nonterminal none some =
  none.right = []
  && symbols_of_production some = []


let right_symbol head_tail =
  symbols_of_production head_tail
  |> List.rev
  |> List.hd


let nonterm_type = function
  | None ->
      <:ctyp<t>>
  | Some name ->
      <:ctyp<$uid:name$.t>>

let merge name = function
  | { params = [l; r] as params } ->
      `SEM_MERGE {
        params;
        code = <:expr<$uid:name$.Merge ($lid:l$, $lid:r$)>>
      }
  | _ ->
      failwith "invalid merge function"


let nonterminal variant reachable nonterm =
  let is_reachable = NtSet.mem reachable nonterm.nbase.index_id in

  let semtype =
    if not is_reachable then
      <:ctyp<unit>>
    else if Ids.Nonterminal.is_start nonterm.nbase.index_id then
      (* synthesised start symbol *)
      nonterm_type None
    else
      nonterm_type (Some nonterm.nbase.name)
  in

  let semantic =
    SemanticVariant.of_list variant [
      Some (`SEM_TYPE semtype);
      if not is_reachable then
        None
      else
        BatOption.map (merge nonterm.nbase.name)
          (Semantic.merge_of_nonterm SemanticVariant.User nonterm);
    ]
  in

  let semantic =
    SemanticVariant.combine nonterm.nbase.semantic semantic
  in

  { nonterm with nbase = { nonterm.nbase with semantic } }


let nonterms variant reachable nonterms =
  NtArray.map (nonterminal variant reachable) nonterms


let check_noname nonterms prod =
  match prod.pbase.name with
  | "" -> ()
  | name ->
      let left = NtArray.get nonterms prod.left in
      failwith (
        "uselessly defined production name \"" ^ name ^ "\"" ^
        " in nonterminal \"" ^ left.nbase.name ^ "\"")


let set_action variant prod action =
  let semantic =
    SemanticVariant.set_list variant [`SEM_ACTION action] prod.pbase.semantic
  in
  { prod with pbase = { prod.pbase with semantic } }


let map_prods variant nonterms reachable has_merge prods =
  match prods with
  | [prod] when is_singleton_nonterminal prod && not has_merge ->
      check_noname nonterms prod;

      let tag =
        symbols_of_production prod
        |> List.hd
        |> GrammarUtil.tag_of_symbol
      in
      let action =
        <:expr<$lid:tag$>>
      in

      [set_action variant prod action]

  | [tail; head_tail] when is_list_nonterminal tail head_tail && not has_merge ->
      check_noname nonterms tail;
      check_noname nonterms head_tail;
      begin match symbols_of_production tail, symbols_of_production head_tail with
      (* possibly empty list *)
      | [], [head2; tail2] ->
          let head2_tag = GrammarUtil.tag_of_symbol head2 in
          let tail2_tag = GrammarUtil.tag_of_symbol tail2 in
          [
            set_action variant tail <:expr<[]>>;
            set_action variant head_tail <:expr<$lid:tail2_tag$ :: $lid:head2_tag$>>;
          ]
      (* non-empty list *)
      | [tail1], [head2; tail2] ->
          let tail1_tag = GrammarUtil.tag_of_symbol tail1 in
          let head2_tag = GrammarUtil.tag_of_symbol head2 in
          let tail2_tag = GrammarUtil.tag_of_symbol tail2 in
          [
            set_action variant tail <:expr<[$lid:tail1_tag$]>>;
            set_action variant head_tail <:expr<$lid:tail2_tag$ :: $lid:head2_tag$>>;
          ]

      | _ -> failwith "error in is_list_nonterminal"
      end

  | [none; some] when is_option_nonterminal none some && not has_merge ->
      begin match symbols_of_production some with
      | [some_sym] ->
          let some_tag = GrammarUtil.tag_of_symbol some_sym in
          [
            set_action variant none <:expr<None>>;
            set_action variant some <:expr<Some $lid:some_tag$>>;
          ]

      | _ -> failwith "error in is_option_nonterminal"
      end

  | [none; some] when is_boolean_nonterminal none some && not has_merge ->
      [
        set_action variant none <:expr<false>>;
        set_action variant some <:expr<true>>;
      ]

  | prods ->
      List.map (fun prod ->
        let action =
          (* production 0 is the synthesised start symbol *)
          if Ids.Production.is_start prod.pbase.index_id then (
            <:expr<top>>
          ) else (
            let prod_name =
              match prod.pbase.name with
              | ""   -> "P" ^ Ids.Production.to_string prod.pbase.index_id
              | name -> assert (name <> ""); name
            in

            let prod_variant =
              let left = NtArray.get nonterms prod.left in
              <:expr<$uid:left.nbase.name$.$uid:prod_name$>>
            in

            let args =
              List.fold_left (fun args -> function
                | Nonterminal ("", _)
                | Terminal ("", _) ->
                    (* nothing to do for untagged symbols *)
                    args

                | Nonterminal (tag, _)
                | Terminal (tag, _) ->
                    <:expr<$lid:tag$>> :: args
              ) [ <:expr<(start_p, end_p)>> ] prod.right
              |> List.rev
            in

            List.fold_left (fun ctor arg ->
              <:expr<$ctor$ $arg$>>
            ) prod_variant args
          )
        in

        set_action variant prod action
      ) prods


let unit_prods variant nonterms =
  List.map (fun prod ->
    check_noname nonterms prod;
    set_action variant prod <:expr<()>>
  )


(* XXX: if this function changes its output, EmitPtree.production_types probably
 * also needs to change *)
let prods variant reachable nonterms prods_by_lhs prods =
  let prods_by_lhs =
    NtArray.map (fun indices ->
      let prods = List.map (ProdArray.get prods) indices in

      let is_reachable =
        match prods with
        | { left } :: _ -> NtSet.mem reachable left
        | _ -> false
      in

      let has_merge =
        match prods with
        | { left } :: _ ->
            let left = NtArray.get nonterms left in
            Semantic.merge_of_nonterm SemanticVariant.User left != None
        | _ ->
            false
      in

      if is_reachable then
        map_prods variant nonterms reachable has_merge prods
      else
        unit_prods variant nonterms prods
    ) prods_by_lhs
  in

  (* make a new indexed prods *)
  let prod_count = NtArray.sum (List.length) prods_by_lhs in
  let prods = ProdArray.make prod_count empty_production in
  NtArray.iter (List.iter (fun prod -> ProdArray.set prods prod.pbase.index_id prod)) prods_by_lhs;

  ProdArray.readonly prods


let verbatims variant =
  let prefix = SemanticVariant.prefix_for_variant_kind variant in
  SemanticVariant.of_list variant [
    Some (`SEM_VERBATIM      [ <:sig_item< open $uid:Options._module_prefix () ^ prefix$.Ptree >> ]);
    Some (`SEM_IMPL_VERBATIM [ <:str_item< open $uid:Options._module_prefix () ^ prefix$.Ptree >> ]);
  ]
