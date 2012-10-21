open AnalysisEnvType
open GrammarType

let (|>) = BatPervasives.(|>)


let rec compute_reachable_dfs prods prods_by_lhs nonterm =
  (* if we did not see this nonterminal, yet *)
  if not nonterm.nbase.reachable then (
    nonterm.nbase.reachable <- true;

    (* iterate over this nonterminal's rules *)
    List.iter (fun prod_index ->
      let prod = ProdArray.get prods prod_index in

      (* iterate over symbols in the rule RHS *)
      List.iter (function
        | Nonterminal (_, nonterm) ->
            (* recursively analyze nonterminal elements *)
            compute_reachable_dfs prods prods_by_lhs nonterm
        | Terminal (_, term) ->
            (* just mark terminals *)
            term.tbase.reachable <- true
      ) prod.right
    ) (NtArray.get prods_by_lhs nonterm.nt_index)
  )


let compute_reachable nonterms terms prods prods_by_lhs start =
  (* start by clearing the reachability flags *)
  NtArray.  iter (fun nonterm -> nonterm.nbase.reachable <- false) nonterms;
  TermArray.iter (fun    term ->    term.tbase.reachable <- false)    terms;

  (* do a DFS on the grammar, marking things reachable as
   * they're encountered *)
  compute_reachable_dfs prods prods_by_lhs start;

  (* the empty and start symbol are reachable *)
  let empty = NtArray.get nonterms StateId.Nonterminal.empty in
  assert (empty == empty_nonterminal);
  assert (empty.nbase.reachable = false);
  empty.nbase.reachable <- true;

  let start = NtArray.get nonterms StateId.Nonterminal.start in
  assert (start.nbase.name == GrammarTreeParser.start_name);
  assert (start.nbase.reachable = false);
  start.nbase.reachable <- true;

  (* the EOF token is reachable *)
  let eof = TermArray.get terms StateId.Terminal.eof in
  assert (eof.tbase.reachable = false);
  eof.tbase.reachable <- true



let rec compute_reachable_tagged_dfs prods prods_by_lhs reachable prod_index =
  let prod = ProdArray.get prods prod_index in

  let reachable =
    StringSet.add prod.left.nbase.name reachable
  in

  List.fold_left (fun reachable -> function
    (* untagged nonterminals are ignored *)
    | Nonterminal ("", _)
    | Terminal _ ->
        reachable

    | Nonterminal (_, { nt_index; nbase = { name } }) ->
        if StringSet.mem name reachable then
          (* this nonterminal is already reachable *)
          reachable
        else
          (* recurse into the nonterminal's productions *)
          List.fold_left
            (compute_reachable_tagged_dfs prods prods_by_lhs)
            reachable
            (NtArray.get prods_by_lhs nt_index)
  ) reachable prod.right


let compute_reachable_tagged prods prods_by_lhs =
  (* start at the first production *)
  let first_production =
    StateId.Nonterminal.start
    |> NtArray.get prods_by_lhs
    |> List.hd
  in

  compute_reachable_tagged_dfs prods prods_by_lhs StringSet.empty first_production
