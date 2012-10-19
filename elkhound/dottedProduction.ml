open AnalysisEnvType

(************************************************************
 * :: Common Operations
 ************************************************************)


module M = struct
  type t = dotted_production

  let hash a =
    a.dprod_id

  let compare a b =
    a.dprod_id - b.dprod_id

  let equal a b =
    a == b

  let stats _ = failwith "Not supported"
  let reset _ = failwith "Not supported"

  let sexp_of_t = sexp_of_dotted_production
  let t_of_sexp = dotted_production_of_sexp

end

module Table = Hashtbl.Make(M)
module Map = SexpMap.Make(M)
module Set = SexpSet.Make(M)
module Stack = HashStack.Make(Table)


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
