open AnalysisEnvType
open GrammarType


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
  Array.iter (fun nonterm -> nonterm.nbase.reachable <- false) nonterms;
  Array.iter (fun    term ->    term.tbase.reachable <- false)    terms;

  (* do a DFS on the grammar, marking things reachable as
   * they're encountered *)
  compute_reachable_dfs prods prods_by_lhs start;

  (* the empty and start symbol are reachable *)
  assert (nonterms.(0) == empty_nonterminal);
  assert (nonterms.(0).nbase.reachable = false);
  nonterms.(0).nbase.reachable <- true;
  assert (nonterms.(1).nbase.name == GrammarTreeParser.start_name);
  assert (nonterms.(1).nbase.reachable = false);
  nonterms.(1).nbase.reachable <- true;

  (* the EOF token is reachable *)
  assert (terms.(0).tbase.reachable = false);
  terms.(0).tbase.reachable <- true
