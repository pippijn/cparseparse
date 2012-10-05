open AnalysisEnvType
open GrammarType


(* given an LR transition graph, compute the BFS tree on top of it
 * and set the parent links to record the tree *)
let compute_bfs_tree env states =
  (* for the BFS, we need a queue of states yet to be processed, and a
   * pile of 'done' states *)
  let queue = ItemSetStack.create 300 in
  let closed = ItemSetTable.create (List.length states) in

  let is_queued state =
    ItemSetStack.mem queue state
  in

  let is_closed state =
    ItemSetTable.mem closed state
  in

  (* initial entry in queue is root of BFS tree *)
  let start_state = BatOption.get env.start_state in
  assert (start_state == List.hd states);
  ItemSetStack.push queue start_state;

  (* it will be convenient to have all the symbols in a single list
   * for iteration purposes *)
  let all_symbols =
    Array.append
      (Array.map (fun term -> Terminal ("", term)) env.indexed_terms)
      (Array.map (fun nonterm -> Nonterminal ("", nonterm)) env.indexed_nonterms)
  in

  (* loop until the queue is exhausted *)
  while not (ItemSetStack.is_empty queue) do
    (* dequeue first element *)
    let source = ItemSetStack.pop queue in

    (* mark it as done so we won't consider any more transitions to it *)
    ItemSetTable.add closed source ();

    (* for each symbol... *)
    Array.iter (fun sym ->
      (* get the transition on this symbol *)
      let target = ItemSet.transition source sym in

      (* if the target is done or already enqueued, or there is no
       * transition on this symbol, we don't need to consider it
       * further *)
      match target with
      (* no transition *)
      | None -> ()
      (* done *)
      | Some target when is_closed target -> ()
      (* already enqueued *)
      | Some target when is_queued target -> ()

      | Some target ->
          (* the source->target link just examined is the first time
           * we've encounted 'target', so that link becomes the BFS
           * parent link *)
          target.bfs_parent <- Some source;

          (* finally, enqueue the target so we'll explore its targets too *)
          ItemSetStack.push queue target

    ) all_symbols;
  done;

  (* all except the start state should now have a BFS parent *)
  List.iter (fun state ->
    assert (state == start_state || state.bfs_parent != None)
  ) states;

  states
