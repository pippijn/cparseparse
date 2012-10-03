open Gramtype
open AnalysisEnvType


let compute_grammar_properties env grammar =
  (* number of nonterminals + 1 for empty_nonterminal *)
  let nonterm_count = Stringmap.cardinal grammar.nonterminals + 1 in
  let    term_count = Stringmap.cardinal grammar.   terminals     in

  Reachability.compute_reachable env.indexed_nonterms env.indexed_terms env.prods_by_lhs grammar.start_symbol;
  Derivability.compute_derivability_relation env;
  SuperSets.compute_supersets env.indexed_nonterms grammar.nonterminals;
  FirstSets.compute_first env.derivable env.indexed_nonterms env.indexed_prods;
  FirstSets.compute_dprod_first env.derivable env.dotted_prods env.indexed_prods;
  FollowSets.compute_follow env.derivable env.indexed_prods


let run_analyses grammar =
  let env = AnalysisEnv.init_env grammar in

  compute_grammar_properties env grammar;

  (*Bit2d.print env.derivable;*)

  ()
