open AnalysisEnvType
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


let merge name = function
  | None -> None
  | Some { params = [l; r] as params } ->
      Some {
        params;
        code = <:expr<$uid:Options._module_prefix () ^ "Ptree"$.$uid:name$.Merge ($lid:l$, $lid:r$)>>
      }
  | _ -> failwith "invalid merge function"


let nonterminal nonterm =
  let semtype =
    if StateId.Nonterminal.is_start nonterm.nt_index then
      (* synthesised start symbol *)
      <:ctyp<$uid:Options._module_prefix () ^ "Ptree"$.t>>
    else
      <:ctyp<$uid:Options._module_prefix () ^ "Ptree"$.$uid:nonterm.nbase.name$.t>>
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
    merge = merge nonterm.nbase.name nonterm.merge;
  }


let symbol = function
  | Nonterminal (tag, nonterm) -> Nonterminal (tag, nonterminal nonterm)
  | term -> term


let nonterms nonterms =
  Array.map nonterminal nonterms


let check_noname prod =
  match prod.prod_name with
  | None -> ()
  | Some name -> failwith ("uselessly defined production name \"" ^ name ^ "\"")


(* XXX: if this function changes its output, EmitPtree.production_types probably
 * also needs to change *)
let prods_by_lhs prods_by_lhs =
  Array.map (fun prods ->
    let has_merge =
      match prods with
      | { left = { merge = Some _ } } :: _ -> true
      | _ -> false
    in

    match prods with
    (* nonterminal with a single production that is a tagged terminal *)
    | [{ right = [Terminal (tag, _) | Nonterminal (tag, _)] } as prod] when tag <> "" && not has_merge ->
        check_noname prod;
        [{ prod with action = Some <:expr<$lid:tag$>> }]

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
          let left = nonterminal prod.left in
          let right = List.map symbol prod.right in

          let action =
            (* production 0 is the synthesised start symbol *)
            if prod.prod_index = 0 then (
              <:expr<top>>
            ) else (
              let prod_name =
                match prod.prod_name with
                | None      -> "P" ^ string_of_int prod.prod_index
                | Some name -> assert (name <> ""); name
              in

              let prod_variant =
                <:expr<$uid:Options._module_prefix () ^ "Ptree"$.$uid:left.nbase.name$.$uid:prod_name$>>
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
  ) prods_by_lhs
