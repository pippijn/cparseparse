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


module Make(T : sig
  val prefix : string
end) = struct

  let expr_prefix =
    <:expr<$uid:Options._module_prefix () ^ T.prefix$.Ptree>>

  let nonterm_type = function
    | None ->
        <:ctyp<$uid:Options._module_prefix () ^ T.prefix$.Ptree.t>>
    | Some name ->
        <:ctyp<$uid:Options._module_prefix () ^ T.prefix$.Ptree.$uid:name$.t>>

  let merge name = function
    | None -> None
    | Some { params = [l; r] as params } ->
        Some {
          params;
          code = <:expr<$expr_prefix$.$uid:name$.Merge ($lid:l$, $lid:r$)>>
        }
    | _ -> failwith "invalid merge function"


  let nonterminal reachable nonterm =
    let is_reachable = StringSet.mem nonterm.nbase.name reachable in

    let semtype =
      if not is_reachable then
        <:ctyp<unit>>
      else if StateId.Nonterminal.is_start nonterm.nt_index then
        (* synthesised start symbol *)
        nonterm_type None
      else
        nonterm_type (Some nonterm.nbase.name)
    in
    { nonterm with
      nbase = { nonterm.nbase with
        semtype = Some semtype;
        (* most function become trivial *)
        dup = None;
        del = None;
      };
      keep = None;
      (* merge creates a merge node *)
      merge = (if not is_reachable then None else merge nonterm.nbase.name nonterm.merge);
    }


  let symbol reachable = function
    | Nonterminal (tag, nonterm) -> Nonterminal (tag, nonterminal reachable nonterm)
    | term -> term


  let nonterms reachable nonterms =
    NtArray.map (nonterminal reachable) nonterms


  let check_noname prod =
    match prod.prod_name with
    | None -> ()
    | Some name -> failwith ("uselessly defined production name \"" ^ name ^ "\"")


  let map_prods reachable has_merge prods =
    match prods with
    | [prod] when is_singleton_nonterminal prod && not has_merge ->
        check_noname prod;
        let left = nonterminal reachable prod.left in
        let right = List.map (symbol reachable) prod.right in

        let tag =
          symbols_of_production prod
          |> List.hd
          |> GrammarUtil.tag_of_symbol
        in
        let action =
          <:expr<$lid:tag$>>
        in

        [{ prod with left; right; action = Some action }]

    | [tail; head_tail] when is_list_nonterminal tail head_tail && not has_merge ->
        check_noname tail;
        check_noname head_tail;
        begin match symbols_of_production tail, symbols_of_production head_tail with
        (* possibly empty list *)
        | [], [head2; tail2] ->
            let head2_tag = GrammarUtil.tag_of_symbol head2 in
            let tail2_tag = GrammarUtil.tag_of_symbol tail2 in
            [
              { tail with action = Some <:expr<[]>> };
              { head_tail with action = Some <:expr<$lid:tail2_tag$ :: $lid:head2_tag$>> };
            ]
        (* non-empty list *)
        | [tail1], [head2; tail2] ->
            let tail1_tag = GrammarUtil.tag_of_symbol tail1 in
            let head2_tag = GrammarUtil.tag_of_symbol head2 in
            let tail2_tag = GrammarUtil.tag_of_symbol tail2 in
            [
              { tail with action = Some <:expr<[$lid:tail1_tag$]>> };
              { head_tail with action = Some <:expr<$lid:tail2_tag$ :: $lid:head2_tag$>> };
            ]

        | _ -> failwith "error in is_list_nonterminal"
        end

    | [none; some] when is_option_nonterminal none some && not has_merge ->
        begin match symbols_of_production some with
        | [some_sym] ->
            let some_tag = GrammarUtil.tag_of_symbol some_sym in
            [
              { none with action = Some <:expr<None>> };
              { some with action = Some <:expr<Some $lid:some_tag$>> };
            ]

        | _ -> failwith "error in is_option_nonterminal"
        end

    | [none; some] when is_boolean_nonterminal none some && not has_merge ->
        [
          { none with action = Some <:expr<false>> };
          { some with action = Some <:expr<true>> };
        ]

    | prods ->
        List.map (fun prod ->
          let left = nonterminal reachable prod.left in
          let right = List.map (symbol reachable) prod.right in

          let action =
            (* production 0 is the synthesised start symbol *)
            if StateId.Production.is_start prod.prod_index then (
              <:expr<top>>
            ) else (
              let prod_name =
                match prod.prod_name with
                | None      -> "P" ^ StateId.Production.to_string prod.prod_index
                | Some name -> assert (name <> ""); name
              in

              let prod_variant =
                <:expr<$expr_prefix$.$uid:left.nbase.name$.$uid:prod_name$>>
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

          { prod with left; right; action = Some action; }
        ) prods


  let unit_prods =
    List.map (fun prod ->
      let left = nonterminal StringSet.empty prod.left in
      let right = List.map (symbol StringSet.empty) prod.right in
      { prod with left; right; action = Some <:expr<()>> }
    )


  (* XXX: if this function changes its output, EmitPtree.production_types probably
   * also needs to change *)
  let prods reachable prods_by_lhs prods =
    let prods_by_lhs =
      NtArray.map (fun indices ->
        let prods = List.map (ProdArray.get prods) indices in

        let is_reachable =
          match prods with
          | { left = { nbase = { name } } } :: _ -> StringSet.mem name reachable
          | _ -> false
        in

        let has_merge =
          match prods with
          | { left = { merge = Some _ } } :: _ -> true
          | _ -> false
        in

        if is_reachable then
          map_prods reachable has_merge prods
        else
          unit_prods prods
      ) prods_by_lhs
    in

    (* make a new indexed prods *)
    let prod_count = NtArray.sum (List.length) prods_by_lhs in
    let prods = ProdArray.make prod_count empty_production in
    NtArray.iter (List.iter (fun prod -> ProdArray.set prods prod.prod_index prod)) prods_by_lhs;

    prods

end
