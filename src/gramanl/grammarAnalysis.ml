open GrammarType
open AnalysisEnvType

let (|>) = BatPervasives.(|>)


let compute_grammar_properties env grammar =
  Timing.progress "reachability computation"
    (Reachability.compute_reachable env.indexed_nonterms env.indexed_terms env.prods_by_lhs) grammar.start_symbol;
  Timing.progress "derivability computation"
    Derivability.compute_derivability_relation env;
  Timing.progress "super sets computation"
    (SuperSets.compute_supersets env.indexed_nonterms) grammar.nonterminals;
  Timing.progress "first sets computation"
    (FirstSets.compute_first env.derivable env.indexed_nonterms env.indexed_prods) env.indexed_terms;
  Timing.progress "computation of dotted production first sets"
    (FirstSets.compute_dprod_first env.derivable env.dotted_prods env.indexed_prods) env.indexed_terms;
  Timing.progress "follow sets computation"
    FollowSets.compute_follow env.derivable env.indexed_prods


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

  let states, tables = compute_lr_tables env in

  begin
    let unr_nonterms =
      Array.fold_left (fun count nonterm ->
        if not nonterm.nbase.reachable then (
          count + 1
        ) else (
          count
        )
      ) 0 env.indexed_nonterms
    in
    let unr_terms =
      Array.fold_left (fun count term ->
        if not term.tbase.reachable then (
          count + 1
        ) else (
          count
        )
      ) 0 env.indexed_terms
    in

    Warnings.report_unexpected unr_nonterms env.options.expectedUNRNonterms "unreachable nonterminals";
    Warnings.report_unexpected unr_terms env.options.expectedUNRTerms "unreachable terminals";
  end;

  env, states, tables
