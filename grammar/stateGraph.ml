open AnalysisEnvType

module G = Graph.Imperative.Digraph.ConcreteLabeled(ItemSet.M)(ItemSet.M)


let item_set_graph states =
  let g = G.create () in
  List.iter (fun state ->
    Array.iter (function
      | None -> ()
      | Some target ->
          G.add_edge g state target
    ) state.nonterm_transition
  ) states;

  g


module Dot = Graph.Graphviz.Dot(struct
    include G
    let graph_attributes _ = []
    let default_vertex_attributes _ = []

    let vertex_name state = "S" ^ string_of_int (int_of_state_id state.state_id)
    let vertex_attributes _ = []

    let get_subgraph _ = None

    let default_edge_attributes _ = []
    let edge_attributes _ = []
  end)

let dot formatter g =
  Dot.fprint_graph formatter g


let visualise states =
  let g = item_set_graph states in
  let out = open_out "automaton.dot" in
  let formatter = Format.formatter_of_out_channel out in
  dot formatter g;
  close_out out
