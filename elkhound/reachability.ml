open Gramtype


let rec compute_reachable_dfs prods_by_lhs nonterm =
  (* if we did not see this nonterminal, yet *)
  if not nonterm.nbase.reachable then (
    nonterm.nbase.reachable <- true;

    (* iterate over this nonterminal's rules *)
    List.iter (fun prod ->
      (* iterate over symbols in the rule RHS *)
      List.iter (function
        | Nonterminal (_, nonterm) ->
            (* recursively analyze nonterminal elements *)
            compute_reachable_dfs prods_by_lhs nonterm
        | Terminal (_, term) ->
            (* just mark terminals *)
            term.tbase.reachable <- true
      ) prod.right
    ) prods_by_lhs.(nonterm.nt_index)
  )


let compute_reachable nonterms terms prods_by_lhs start =
  (* start by clearing the reachability flags *)
  Array.iter (fun nonterm -> nonterm.nbase.reachable <- false) nonterms;
  Array.iter (fun    term ->    term.tbase.reachable <- false)    terms;

  (* do a DFS on the grammar, marking things reachable as
   * they're encountered *)
  compute_reachable_dfs prods_by_lhs start
