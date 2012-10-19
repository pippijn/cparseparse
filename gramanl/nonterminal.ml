open GrammarType

(************************************************************
 * :: Structure defining operations
 ************************************************************)

module M : GrammarSig.S with type t = nonterminal = struct

  type t = nonterminal

  let hash a =
    a.nt_index

  let compare a b =
    a.nt_index - b.nt_index

  let equal a b =
    a.nt_index = b.nt_index

  let stats _ = failwith "Not supported"
  let reset _ = failwith "Not supported"

  let sexp_of_t = sexp_of_nonterminal
  let t_of_sexp = nonterminal_of_sexp

  let default = empty_nonterminal

end

module Table = Hashtbl.Make(M)
module Map   = SexpMap.Make(M)
module Set   = SexpSet.Make(M)
module Stack = HashStack.Make(Table)
module Graph = Graph.Persistent.Digraph.ConcreteLabeled(M)(M)


(************************************************************
 * :: Compute graph over nonterminals
 ************************************************************)

let compute_graph =
  (* fold over productions *)
  List.fold_left (fun g prod ->
    let left = prod.left in
    (* fold over rhs *)
    List.fold_left (fun g -> function
      | Nonterminal (_, right) ->
          Graph.add_edge g left right
      | _ ->
          g
    ) g prod.right
  ) Graph.empty
