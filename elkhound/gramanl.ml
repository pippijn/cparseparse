open GrammarType
open AnalysisEnvType


let compute_grammar_properties env grammar =
  Reachability.compute_reachable env.indexed_nonterms env.indexed_terms env.prods_by_lhs grammar.start_symbol;
  Derivability.compute_derivability_relation env;
  SuperSets.compute_supersets env.indexed_nonterms grammar.nonterminals;
  FirstSets.compute_first env.derivable env.indexed_nonterms env.indexed_prods;
  FirstSets.compute_dprod_first env.derivable env.dotted_prods env.indexed_prods;
  FollowSets.compute_follow env.derivable env.indexed_prods


let compute_lr_tables env =
  Timing.time LrItemSets.construct_lr_item_sets env;

  ()


let run_analyses grammar =
  let env = AnalysisEnv.init_env grammar in

  compute_grammar_properties env grammar;

  compute_lr_tables env;

  (*Bit2d.print env.derivable;*)

  ()
