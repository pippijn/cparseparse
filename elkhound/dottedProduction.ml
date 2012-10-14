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


let symbol_before_dot dprod =
  let open GrammarType in
  List.nth dprod.prod.right (dprod.dot - 1)


let symbol_after_dot dprod =
  dprod.after_dot


let is_dot_at_start dprod =
  dprod.dot = 0


let is_dot_at_end dprod =
  symbol_after_dot dprod == None


let back_pointer dprod =
  match dprod.back_pointer with
  | None -> failwith "no back_pointer"
  | Some item -> item
