open Gramtype
open AnalysisEnvType


let run_analyses grammar =
  let env = AnalysisEnv.init_env grammar in

  Reachability.compute_reachable env.indexed_nonterms env.indexed_terms env.prods_by_lhs grammar.start_symbol;
  Derivability.compute_derivability_relation env;
  SuperSets.compute_supersets env.indexed_nonterms grammar.nonterminals;
  FirstSets.compute_first env.indexed_nonterms;

  (*Bit2d.print env.derivable;*)

  ()
