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
    Timing.time "LR item sets construction" LrItemSets.construct_lr_item_sets env
    |> Timing.time "renumbering states" (Renumbering.renumber_states env)
    |> Timing.time "BFS tree on transition graph" (BfsTree.compute_bfs_tree env)
    |> Timing.time "parse table construction" (TableConstruction.compute_parse_tables env (*~allow_ambig:*)true)
  in

  (*
  List.iter (fun item_set ->
    PrintAnalysisEnv.print_item_set env item_set;
    print_newline ()
  ) states;
  *)

  ()


let run_analyses grammar =
  let env = AnalysisEnv.init_env grammar in

  compute_grammar_properties env grammar;

  compute_lr_tables env;

  (*Bit2d.print env.derivable;*)

  ()
