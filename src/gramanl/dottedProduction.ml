open AnalysisEnvType

(************************************************************
 * :: Structure defining operations
 ************************************************************)

module M : GrammarSig.S with type t = dotted_production = struct
  type t = dotted_production

  let hash a =
    a.dprod_id

  let compare a b =
    assert (a == b || a.dprod_id != b.dprod_id);
    a.dprod_id - b.dprod_id

  let equal a b =
    assert (a == b || a.dprod_id != b.dprod_id);
    a == b

  let stats _ = failwith "Not supported"
  let reset _ = failwith "Not supported"

  let sexp_of_t = sexp_of_dotted_production
  let t_of_sexp = dotted_production_of_sexp

  let default = {
    dprod_id = -1;
    prod = GrammarType.empty_production;
    dot = -1;
    after_dot = None;
    first_set = TerminalSet.empty;
    can_derive_empty = false;
    back_pointer = None;
  }

end

module Table = Hashtbl.Make(M)
module Map   = SexpMap.Make(M)
module Set   = SexpSet.Make(M)
module Stack = HashStack.Make(Table)
module Graph = Graph.Persistent.Digraph.ConcreteLabeled(M)(M)


(************************************************************
 * :: Functions
 ************************************************************)


let get dotted_prods prod dot =
  let open GrammarType in
  (ProdArray.get dotted_prods prod.prod_index).(dot)


let next dotted_prods dprod =
  let open GrammarType in
  (ProdArray.get dotted_prods dprod.prod.prod_index).(dprod.dot + 1)


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
