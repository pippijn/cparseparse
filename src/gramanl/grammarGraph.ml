open GrammarType

module G = Nonterminal.Graph


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
  let g = Nonterminal.compute_graph grammar.productions in
  let out = open_out "grammar.dot" in
  let formatter = Format.formatter_of_out_channel out in
  dot formatter g;
  close_out out