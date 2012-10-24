open GrammarType
open AnalysisEnvType

let (|>) = BatPervasives.(|>)


let compute_reachable env grammar =
  let reachable_nonterms, reachable_terms =
    Reachability.compute_reachable
      env.index.nonterms
      env.index.terms
      env.index.prods
      env.prods_by_lhs
      grammar.start_symbol
  in

  let unr_nonterms =   NtArray.length env.index.nonterms -   NtSet.cardinal reachable_nonterms in
  let unr_terms    = TermArray.length env.index.terms    - TermSet.cardinal reachable_terms in

  begin
    Warnings.report_unexpected unr_nonterms env.options.expectedUNRNonterms "unreachable nonterminals";
    Warnings.report_unexpected unr_terms env.options.expectedUNRTerms "unreachable terminals";
  end



let compute_grammar_properties env grammar =
  Timing.progress "reachability computation"
    (compute_reachable env) grammar;
  Timing.progress "derivability computation"
    Derivability.compute_derivability_relation env;
  Timing.progress "super sets computation"
    (SuperSets.compute_supersets env.index.nonterms) grammar.nonterminals;
  Timing.progress "first sets computation"
    (FirstSets.compute_first env.derivable env.index.nonterms env.index.prods) env.index.terms;
  Timing.progress "computation of dotted production first sets"
    (FirstSets.compute_dprod_first env.derivable env.dotted_prods env.index.prods) env.index.terms;
  Timing.progress "follow sets computation"
    FollowSets.compute_follow env.derivable env.index.prods


let compute_lr_tables env =
  let states =
    Timing.progress "LR item set construction" LrItemSets.construct_lr_item_sets env
    |> Timing.progress "renumbering states" (Renumbering.renumber_states env)
    |> Timing.progress "BFS tree on state graph" (BfsTree.compute_bfs_tree env)
  in
  let tables =
    states
    |> Timing.progress "parse table construction" (TableConstruction.compute_parse_tables env (*~allow_ambig:*)true)
  in

  states, tables


let run_analyses grammar =
  let env = AnalysisEnv.init_env grammar in

  compute_grammar_properties env grammar;

  env, compute_lr_tables env
