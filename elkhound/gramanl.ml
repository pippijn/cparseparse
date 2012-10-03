open Gramtype
open AnalysisEnvType


let run_analyses grammar =
  (* number of nonterminals + 1 for empty_nonterminal *)
  let nonterm_count = Stringmap.cardinal grammar.nonterminals + 1 in
  let    term_count = Stringmap.cardinal grammar.   terminals     in

  let env = AnalysisEnv.init_env grammar in

  Reachability.compute_reachable env.indexed_nonterms env.indexed_terms env.prods_by_lhs grammar.start_symbol;
  Derivability.compute_derivability_relation env;
  SuperSets.compute_supersets env.indexed_nonterms grammar.nonterminals;
  FirstSets.compute_first env.derivable env.indexed_nonterms env.indexed_prods;
  Timing.time (FirstSets.compute_dprod_first env.derivable env.dotted_prods) env.indexed_prods;

  (*Bit2d.print env.derivable;*)

  ()
