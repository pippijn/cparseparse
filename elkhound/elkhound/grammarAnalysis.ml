open BatPervasives
open GrammarType
open AnalysisEnvType


let compute_grammar_properties env grammar =
  Reachability.compute_reachable env.indexed_nonterms env.indexed_terms env.prods_by_lhs grammar.start_symbol;
  Derivability.compute_derivability_relation env;
  SuperSets.compute_supersets env.indexed_nonterms grammar.nonterminals;
  FirstSets.compute_first env.derivable env.indexed_nonterms env.indexed_prods env.indexed_terms;
  FirstSets.compute_dprod_first env.derivable env.dotted_prods env.indexed_prods env.indexed_terms;
  FollowSets.compute_follow env.derivable env.indexed_prods env.indexed_terms


let compute_lr_tables env =
  let tables =
    LrItemSets.construct_lr_item_sets env
    |> Renumbering.renumber_states env
    |> BfsTree.compute_bfs_tree env
    |> TableConstruction.compute_parse_tables env (*~allow_ambig:*)true
  in

  tables


let run_analyses grammar =
  let env = AnalysisEnv.init_env grammar in

  compute_grammar_properties env grammar;

  let tables = compute_lr_tables env in

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

  env, tables
