open AnalysisEnvType


let name_of_symbol_opt = function
  | Some sym -> GrammarUtil.name_of_symbol sym
  | None -> "None"


let ordering_operator order =
  if order < 0 then
    "<"
  else if order > 0 then
    ">"
  else
    "="


let compare_by_outgoing syms transition_fn a b =
  ExtArray.foldl_untili (fun t _ ->
    let dest_a = transition_fn a t in
    let dest_b = transition_fn b t in

    match dest_a, dest_b with
    | None, Some _ -> 1
    | Some _, None -> -1
    | Some dest_a, Some dest_b ->
        int_of_state_id dest_a.state_id - int_of_state_id dest_b.state_id
    | _ -> 0
  ) 0 syms


let compare_by_reductions terms a b =
  ExtArray.foldl_until (fun term ->
    let red_a = List.sort compare (ItemSet.possible_reductions a term) in
    let red_b = List.sort compare (ItemSet.possible_reductions b term) in

    compare red_a red_b
  ) 0 terms


let renumber_states_compare env a b =
  let open GrammarType in

  (* order them first by their incoming arc symbol; this affects
   * the renumbering that the Code Reduction Scheme demands *)
  let order = GrammarUtil.compare_symbol a.state_symbol b.state_symbol in

  (* from this point on, the CRS would be happy with an arbitrary
   * order, but I want the state numbering to be canonical so that
   * I have an easier time debugging and comparing parse traces
   *
   * they have the same incoming arc symbol; now, sort by outgoing
   * arc symbols
   *)

  let (|<>) a b = if a <> 0 then a else Lazy.force b in

  let arbitrary_order = a != b && order = 0 in

  let order =
    order
    (* first up: terminals *)
    |<> lazy (compare_by_outgoing env.indexed_terms (fun is t -> is.term_transition.(t)) a b)
    (* next: nonterminals *)
    |<> lazy (compare_by_outgoing env.indexed_nonterms (fun is t -> is.nonterm_transition.(t)) a b)
    (* finally, order by possible reductions *)
    |<> lazy (compare_by_reductions env.indexed_terms a b)
  in

  if Options._trace_renumbering () then (
    if arbitrary_order then (
      Printf.printf "%d[%s] %s %d[%s]\n"
        (int_of_state_id a.state_id)
        (name_of_symbol_opt a.state_symbol)
        (ordering_operator order)
        (int_of_state_id b.state_id)
        (name_of_symbol_opt b.state_symbol);
      PrintAnalysisEnv.print_item_set env a;
      PrintAnalysisEnv.print_item_set env b;
    );
  );

  (* validate invariants *)
  if a != b then (
    assert (order <> 0);
    if int_of_state_id a.state_id = 0 then
      assert (order < 0);
    if int_of_state_id b.state_id = 0 then
      assert (order > 0);
  ) else (
    assert (order = 0);
  );

  order


(* The purpose of this function is to number the states (which have up
 * to this point been numbered arbitrarily) in such a way that all
 * states that have a given symbol on incoming arcs will be numbered
 * consecutively.  This is part of the table compression schemes
 * described in the Dencker et. al. paper (see module Parsetables). *)
let renumber_states env states =
  (* sort them in the right order *)
  let states = List.sort (renumber_states_compare env) states in

  (* number them in that order *)
  BatList.iteri (fun i state ->
    if i = 0 then (
      (* the first element should always be the start state *)
      assert (int_of_state_id state.state_id = 0);
      assert (BatOption.get env.start_state == state);
    );

    state.state_id <- state_id_of_int i;
  ) states;

  if false then (
    List.iter (fun state ->
      PrintAnalysisEnv.print_item_set env state;
    ) states;
  );

  states
