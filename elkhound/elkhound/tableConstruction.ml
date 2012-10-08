open AnalysisEnvType


(* this is a depth-first traversal of the 'derivable' relation;
 * when we reach a nonterminal that can't derive any others not
 * already in the order, we give its entry the latest ordinal
 * that isn't already taken ('next_ordinal') *)
let rec topological_sort nonterm_count (* number of nonterminals in the grammar *)
                         derivable (* derivability graph *)
                         seen (* set of nonterminals we've already seen *)
                         order (* table we're filling with ordinals *)
                         next_ordinal (* latest ordinal not yet used *)
                         current (* current nonterminal to expand *)
                         =
  if BitSet.is_set seen current then (
    (* already expanded this one *)
    next_ordinal
  ) else (
    (* don't expand this one again *)
    BitSet.set seen current;

    (* look at all nonterminals this one can derive *)
    let rec loop nt next_ordinal =
      if nt = nonterm_count then
        next_ordinal
      else if Derivability.can_derive_i derivable nt current then
        (* 'nt' can derive 'current'; expand 'nt' first, thus making
         * it later in the order, so we'll reduce to 'current' before
         * reducing to 'nt' (when token spans are equal) *)
        loop (nt + 1) (topological_sort nonterm_count derivable seen order next_ordinal nt)
      else
        loop (nt + 1) next_ordinal
    in

    let next_ordinal = loop 0 next_ordinal in
    (* finally, put 'current' into the order *)
    order.(current) <- next_ordinal;
    next_ordinal - 1
  )


(* use the derivability relation to compute a total order
 * on nonterminals *)
let topological_order derivable nonterms =
  let open GrammarType in
  let nonterm_count = Array.length nonterms in
  let seen = BitSet.create nonterm_count in

  let order = Array.make nonterm_count 0 in
  ignore (Array.fold_left (fun next_ordinal nonterm ->
    (* expand from 'nt' in case it's disconnected; this will be
     * a no-op if we've already 'seen' it *)
    topological_sort nonterm_count derivable seen order next_ordinal nonterm.nt_index
  ) (nonterm_count - 1) nonterms);

  order


let compute_actions state terminal allow_ambig sr rr =
  (* can shift? *)
  let shift_dest = ItemSet.transition_for_term state terminal in

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

  shift_dest, reductions


let compute_cell_action tables state shift_dest reductions terminal =
  let open GrammarType in

  (* still conflicts? *)
  let actions = ConflictResolution.actions shift_dest reductions in
  if actions >= 2 then (
    (* make a new ambiguous-action entry-set *)
    let shift_action =
      match shift_dest with
      | Some shift_dest ->
          [TableEncoding.encode_shift tables shift_dest.state_id terminal.term_index]
      | None ->
          []
    in

    let reduce_actions =
      List.map (fun prod ->
        TableEncoding.encode_reduce tables prod.prod_index state.state_id
      ) reductions
    in

    let set = shift_action @ reduce_actions in
    assert (List.length set = actions);

    TableEncoding.encode_ambig tables set state.state_id
  ) else (
    (* single action *)
    match shift_dest with
    | Some shift_dest ->
        assert (reductions = []);
        TableEncoding.encode_shift tables shift_dest.state_id terminal.term_index
    | None ->
        match reductions with
        | [] ->
            TableEncoding.encode_error tables
        | [prod] ->
            TableEncoding.encode_reduce tables prod.prod_index state.state_id
        | _ ->
            failwith "logic error"
  )



let encode_symbol_id = let open GrammarType in function
  | None -> 0
  | Some (Terminal (_, term)) -> term.term_index + 1
  | Some (Nonterminal (_, nonterm)) -> -nonterm.nt_index - 1


let calls = ref 0
let encode_goto_row tables state nonterm =
  let open GrammarType in
  incr calls;

  (* where do we go when we reduce to this nonterminal? *)
  let goto_dest = ItemSet.transition_for_nonterm state nonterm in

  let cell_goto =
    match goto_dest with
    | Some goto_dest ->
        TableEncoding.encode_goto tables goto_dest.state_id nonterm.nt_index
    | None ->
        (* this should never be accessed at parse time.. *)
        TableEncoding.encode_goto_error tables
  in

  (* fill in entry *)
  TableEncoding.set_goto_entry tables state.state_id nonterm.nt_index cell_goto


let encode_action tables nonterms allow_ambig sr rr state terminal =
  let open GrammarType in
  (* compute shift/reduce actions *)
  let shift_dest, reductions = compute_actions state terminal allow_ambig sr rr in

  (* what to do in this cell *)
  let cell_action = compute_cell_action tables state shift_dest reductions terminal in

  (* add this entry to the table *)
  TableEncoding.set_action_entry tables state.state_id terminal.term_index cell_action


let compute_action_row env tables nonterms terms allow_ambig sr rr state =
  if Config.trace_conflict then (
    if false then (
      PrintAnalysisEnv.print_item_set env state;
    );
    Printf.printf "------ state %d ------\n" (int_of_state_id state.state_id);
  );

  (* ---- fill in this row in the action table ---- *)

  (* for each possible lookahead... *)
  Array.iter (fun terminal ->
    encode_action tables nonterms allow_ambig sr rr state terminal;
  ) terms;

  (* ---- fill in this row in the goto table ---- *)

  (* for each nonterminal... *)
  Array.iter (fun nonterm ->
    encode_goto_row tables state nonterm
  ) nonterms;

  (* get the state symbol *)
  assert (int_of_state_id state.state_id < TableEncoding.num_states tables);
  TableEncoding.set_state_symbol tables state.state_id (encode_symbol_id state.state_symbol)




let compute_parse_tables env allow_ambig states =
  let open GrammarType in

  if false then (
    Array.iter (fun prod ->
      PrintGrammar.print_production prod;
      print_newline ();
    ) env.indexed_prods;
  );

  let tables = TableEncoding.create
    (Array.length env.indexed_terms)
    (Array.length env.indexed_nonterms)
    (List.length states)
    (Array.length env.indexed_prods)
    (topological_order env.derivable env.indexed_nonterms)
    (BatOption.get env.start_state).state_id
    (*~final_prod:*)0 (* slight hack: assume it's the first production *)
  in

  (* count total number of conflicts of each kind *)
  let sr = ref 0 in
  let rr = ref 0 in

  (* for each state... *)
  List.iter (fun state ->
    compute_action_row env tables env.indexed_nonterms env.indexed_terms allow_ambig sr rr state
  ) states;

  (* report on conflict counts *)
  Warnings.report_unexpected !sr env.options.expectedSR "shift/reduce conflicts";
  Warnings.report_unexpected !rr env.options.expectedRR "reduce/reduce conflicts";

  (* report on cyclicity *)
  Array.iter (fun nonterm ->
    if nonterm.cyclic then
      Printf.printf "grammar symbol %s is cyclic\n"
        nonterm.nbase.name
  ) env.indexed_nonterms;

  (* fill in 'prod_info' *)
  Array.iter (fun prod ->
    TableEncoding.set_prod_info tables prod.prod_index (List.length prod.right) prod.left.nt_index
  ) env.indexed_prods;

  TableEncoding.finish_tables tables
