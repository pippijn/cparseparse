open AnalysisEnvType


let compare_by_outgoing indexed transition_fn a b =
  fst (Array.fold_left (fun (order, t) _ ->
    let order =
      if order <> 0 then
        order
      else
        let dest_a = transition_fn a t in
        let dest_b = transition_fn b t in

        match dest_a, dest_b with
        | None, Some _ -> 1
        | Some _, None -> -1
        | Some dest_a, Some dest_b ->
            int_of_state_id dest_a.state_id - int_of_state_id dest_b.state_id
        | _ -> 0
    in

    order, t + 1
  ) (0, 0) indexed)


let compare_by_reductions terms a b =
  Array.fold_left (fun order term ->
    if order <> 0 then
      order
    else
      let red_a = List.sort compare (ItemSet.possible_reductions a term) in
      let red_b = List.sort compare (ItemSet.possible_reductions b term) in

      compare red_a red_b
  ) 0 terms


let renumber_states_compare env a b =
  let open GrammarType in

  (* order them first by their incoming arc symbol; this affects
   * the renumbering that the Code Reduction Scheme demands *)
  let order = Grammar.compare_symbol a.state_symbol b.state_symbol in

  (* from this point on, the CRS would be happy with an arbitrary
   * order, but I want the state numbering to be canonical so that
   * I have an easier time debugging and comparing parse traces
   *
   * they have the same incoming arc symbol; now, sort by outgoing
   * arc symbols
   *)

  (* first up: terminals *)
  let order =
    if order <> 0 then
      order
    else
      compare_by_outgoing env.indexed_terms (fun is t -> is.term_transition.(t)) a b
  in

  (* next: nonterminals *)
  let order =
    if order <> 0 then
      order
    else
      compare_by_outgoing env.indexed_nonterms (fun is t -> is.nonterm_transition.(t)) a b
  in

  (* finally, order by possible reductions *)
  let order =
    if order <> 0 then
      order
    else
      compare_by_reductions env.indexed_terms a b
  in

  if a != b then (
    assert (order <> 0);
    if int_of_state_id a.state_id = 0 then
      assert (order > 0);
  );

  order


(* The purpose of this function is to number the states (which have up
 * to this point been numbered arbitrarily) in such a way that all
 * states that have a given symbol on incoming arcs will be numbered
 * consecutively.  This is part of the table compression schemes
 * described in the Dencker et. al. paper (see module ParseTables). *)
let renumber_states env states =
  (* sort them in the right order *)
  let states = List.rev (List.sort (renumber_states_compare env) states) in

  (* number them in that order *)
  ignore (
    List.fold_left (fun i state ->
      if i = 0 then (
        (* the first element should always be the start state *)
        assert (int_of_state_id state.state_id = 0);
        assert (BatOption.get env.start_state == state);
      );

      state.state_id <- state_id_of_int i;

      i + 1
    ) 0 states
  );

  states
