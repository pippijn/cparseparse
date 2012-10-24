open AnalysisEnvType
open GrammarType

let (|>) = BatPervasives.(|>)


(************************************************************
 * :: General reachability of grammar elements
 ************************************************************)

let rec compute_reachable_dfs nreach treach prods prods_by_lhs nt_index =
  (* if we did not see this nonterminal, yet *)
  if not (NtSet.mem nreach nt_index) then (
    NtSet.add nreach nt_index;

    (* iterate over this nonterminal's rules *)
    List.iter (fun prod_index ->
      let prod = ProdArray.get prods prod_index in

      (* iterate over symbols in the rule RHS *)
      List.iter (function
        | Nonterminal (_, nonterm) ->
            (* recursively analyze nonterminal elements *)
            compute_reachable_dfs nreach treach prods prods_by_lhs nonterm
        | Terminal (_, term) ->
            (* just mark terminals *)
            TermSet.add treach term
      ) prod.right
    ) (NtArray.get prods_by_lhs nt_index)
  )


let compute_reachable terms prods prods_by_lhs start =
  (* start by creating the reachability bitsets *)
  let nreach = NtSet.create (NtArray.length prods_by_lhs) in
  let treach = TermSet.create (TermArray.length terms) in

  (* do a DFS on the grammar, marking things reachable as
   * they're encountered *)
  compute_reachable_dfs nreach treach prods prods_by_lhs start;

  (* the empty and start symbol are reachable *)
  NtSet.add nreach StateId.Nonterminal.empty;
  NtSet.add nreach StateId.Nonterminal.start;

  (* the EOF token is reachable *)
  TermSet.add treach StateId.Terminal.eof;

  NtSet.readonly nreach,
  TermSet.readonly treach



(************************************************************
 * :: Reachability via tagged symbols
 ************************************************************)

let rec compute_reachable_tagged_dfs reachable prods prods_by_lhs nt_index =
  (* if we did not see this nonterminal, yet *)
  if not (NtSet.mem reachable nt_index) then (
    NtSet.add reachable nt_index;

    (* iterate over this nonterminal's rules *)
    List.iter (fun prod_index ->
      let prod = ProdArray.get prods prod_index in

      (* iterate over symbols in the rule RHS *)
      List.iter (function
        | Nonterminal (tag, nonterm) when tag <> "" ->
            (* recursively analyze nonterminal elements *)
            compute_reachable_tagged_dfs reachable prods prods_by_lhs nonterm

        | _ -> () (* ignore untagged and terminals *)
      ) prod.right
    ) (NtArray.get prods_by_lhs nt_index)
  )


let compute_reachable_tagged prods prods_by_lhs =
  (* start by creating the reachability bitset *)
  let reachable = NtSet.create (NtArray.length prods_by_lhs) in

  (* do a DFS on the grammar, marking things reachable as
   * they're encountered *)
  compute_reachable_tagged_dfs reachable prods prods_by_lhs StateId.Nonterminal.start;

  NtSet.readonly reachable
