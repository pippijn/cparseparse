open GrammarType

module NonterminalVertex : Graph.Sig.COMPARABLE with type t = nonterminal = struct
  type t = nonterminal
  let compare a b = String.compare a.nbase.name b.nbase.name
  let hash a = Hashtbl.hash a.nbase.name
  let equal = (==)
end
module NonterminalLabel : Graph.Sig.ORDERED_TYPE_DFT = struct
  type t = nonterminal
  let default = empty_nonterminal
  let compare a b = String.compare a.nbase.name b.nbase.name
end

module G = Graph.Imperative.Digraph.ConcreteLabeled(NonterminalVertex)(NonterminalLabel)


let nonterminal_graph grammar =
  let g = G.create () in
  List.iter (fun prod ->
    let left = prod.left in
    List.iter (function
      | Nonterminal (_, right) ->
          G.add_edge g left right
      | _ -> ()
    ) prod.right;
  ) grammar.productions;

  g


module Dot = Graph.Graphviz.Dot(struct
    include G
    let graph_attributes _ = []
    let default_vertex_attributes _ = []

    let vertex_name nonterm = nonterm.nbase.name
    let vertex_attributes _ = []

    let get_subgraph _ = None

    let default_edge_attributes _ = []
    let edge_attributes _ = []
  end)

let dot formatter g =
  Dot.fprint_graph formatter g


let visualise grammar =
  let g = nonterminal_graph grammar in
  let out = open_out "grammar.dot" in
  let formatter = Format.formatter_of_out_channel out in
  dot formatter g;
  close_out out
