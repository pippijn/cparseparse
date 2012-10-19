open AnalysisEnvType


module Dot = Graph.Graphviz.Dot(struct
    include ItemSet.Graph
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
  let g = ItemSet.compute_graph states in
  let out = open_out "automaton.dot" in
  let formatter = Format.formatter_of_out_channel out in
  dot formatter g;
  close_out out