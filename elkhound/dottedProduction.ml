open AnalysisEnvType

(************************************************************
 * :: Functions
 ************************************************************)


let empty = empty_dotted_production


let get dotted_prods prod dot =
  let open GrammarType in
  dotted_prods.(prod.prod_index).(dot)


let next dotted_prods dprod =
  let open GrammarType in
  dotted_prods.(dprod.prod.prod_index).(dprod.dot + 1)


let is_dot_at_start dprod =
  assert (dprod.before_dot == None || dprod.dot != 0);
  dprod.before_dot == None


let is_dot_at_end dprod =
  dprod.after_dot == None
