open AnalysisEnvType


let create numTerms numNonterms numStates numProds startState finalProductionIndex =
  Parsetables.({
    numTerms;
    numNonterms;
    numProds;

    numStates;

    actionCols = numTerms;
    actionTable = Array.make (numTerms * numStates) 0;

    gotoCols = numNonterms;
    gotoTable = Array.make (numNonterms * numStates) 0;

    prodInfo_rhsLen = Array.make numProds 0;
    prodInfo_lhsIndex = Array.make numProds 0;

    stateSymbol = Array.make numStates 0;

    (* table of ambiguous actions is empty until someone fills in the
     * whole thing; since we don't know how many there might be, we
     * can't even allocate the storage now *)
    ambigTableSize = 0;
    ambigTable = [||];

    startState;
    finalProductionIndex;

    nontermOrder = Array.make numNonterms 0;
  })


let compute_parse_tables env allow_ambig states =
  let tables = create
    (Array.length env.indexed_terms)
    (Array.length env.indexed_nonterms)
    (List.length states)
    (Array.length env.indexed_prods)
    (int_of_state_id (BatOption.get env.start_state).state_id)
    (*~final_prod:*)0 (* slight hack: assume it's the first production *)
  in

  (* count total number of conflicts of each kind *)
  let sr = ref 0 in
  let rr = ref 0 in

  (* for each state... *)
  List.iter (fun state ->
    (* ---- fill in this row in the action table ---- *)
    if Config.trace_conflict then (
      PrintAnalysisEnv.print_item_set env state;
      Printf.printf "------ state %d ------\n" (int_of_state_id state.state_id);
    );

    (* for each possible lookahead... *)
    Array.iter (fun terminal ->
      (* can shift? *)
      let shift_dest = ItemSet.transition state (GrammarType.Terminal ("", terminal)) in

      (* can reduce? *)
      let reductions = ItemSet.possible_reductions state terminal in

      let sr_old = !sr in
      let rr_old = !rr in

      (* try to resolve conflicts; this may print warnings about
       * the conflicts, depending on various factors; if 'allow_ambig'
       * is false, this will remove all but one action *)
      let shift_dest, reductions =
        ConflictResolution.resolve_conflicts state terminal shift_dest reductions allow_ambig sr rr
      in

      if Config.trace_conflict then (
        if sr_old <> !sr || rr_old <> !rr then
          Printf.printf "%d / %d\n" !sr !rr;
      );

      (* what to do in this cell *)
      (*let actions =*)

      ()
    ) env.indexed_terms;


    ()
  ) states;

  ()
