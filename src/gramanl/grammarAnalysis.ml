open GrammarType
open AnalysisEnvType

let (|>) = BatPervasives.(|>)


let compute_reachable env =
  let reachable_nonterms, reachable_terms =
    Timing.progress "reachability computation"
      (Reachability.compute_reachable env.index.terms env.index.prods env.prods_by_lhs) env.start_nt
  in

  let unr_nonterms =   NtArray.length env.index.nonterms -   NtSet.cardinal reachable_nonterms in
  let unr_terms    = TermArray.length env.index.terms    - TermSet.cardinal reachable_terms in

  if false then (
    NtArray.iter (fun nonterm ->
      if not (NtSet.mem reachable_nonterms nonterm.nbase.index_id) then
        Printf.printf "unreachable nonterminal: %s\n" nonterm.nbase.name
    ) env.index.nonterms;
    TermArray.iter (fun term ->
      if not (TermSet.mem reachable_terms term.tbase.index_id) then
        Printf.printf "unreachable terminal: %s\n" term.tbase.name
    ) env.index.terms;
  );

  begin
    Warnings.report_unexpected unr_nonterms env.options.expectedUNRNonterms "unreachable nonterminals";
    Warnings.report_unexpected unr_terms env.options.expectedUNRTerms "unreachable terminals";
  end



let compute_grammar_properties env grammar =
  compute_reachable env;

  Timing.progress "derivability computation"
    Derivability.compute_derivability_relation env;
  Timing.progress "super sets computation"
    (SuperSets.compute_supersets env.index.nonterms) grammar.nonterminals;
  Timing.progress "first sets computation"
    (FirstSets.compute_first env.derivable) env.index;
  Timing.progress "computation of dotted production first sets"
    (FirstSets.compute_dprod_first env.derivable env.dotted_prods) env.index;
  Timing.progress "follow sets computation"
    (FollowSets.compute_follow env.derivable) env.index


let compute_lr_tables env =
  let states =
    Timing.progress "LR item set construction" LrItemSets.construct_lr_item_sets env
    |> Timing.progress "renumbering states" (Renumbering.renumber_states env)
    |> Timing.progress "BFS tree on state graph" (BfsTree.compute_bfs_tree env)
  in
  let tables =
    states
    |> Timing.progress "parse table construction"
         (TableConstruction.compute_parse_tables env (*~allow_ambig:*)true)
  in

  states, tables


let run_analyses grammar =
  let env = AnalysisEnv.init_env grammar in

  compute_grammar_properties env grammar;

  env, compute_lr_tables env
