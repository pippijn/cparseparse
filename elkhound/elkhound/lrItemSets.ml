open BatPervasives
open AnalysisEnvType


type env = {
  env : AnalysisEnvType.env;
  nonterm_count : int;
  term_count    : int;

  mutable next_item_set_id : int;

  item_sets_pending : item_set ItemSetStack.t;
  mutable item_sets_done : item_set ItemSetMap.t;
}


let make_item_set ?(hash=0) env kernel_items =
  let state_id = state_id_of_int env.next_item_set_id in
  env.next_item_set_id <- env.next_item_set_id + 1;

  let hash =
    if hash <> 0 then
      hash
    else
      ItemSetS.hash_kernel_items kernel_items
  in

  {
    kernel_items;
    nonkernel_items = [];
    term_transition = Array.make env.term_count None;
    nonterm_transition = Array.make env.nonterm_count None;
    dots_at_end = [];
    state_symbol = None;
    state_id = state_id;
    bfs_parent = None;
    hash;
  }


let changed_items item_set =
  (* recompute dots_at_end *)
  item_set.dots_at_end <-
    ListUtil.fold_left_many (fun dots_at_end item ->
      if LrItem.is_dot_at_end item then
        (* dot is at end *)
        item :: dots_at_end
      else
        (* dot is not at end *)
        dots_at_end
    ) [] [item_set.kernel_items; item_set.nonkernel_items];

  (* compute this so we can throw away items later if we want to *)
  item_set.state_symbol <-
    try
      (* need only check kernel items since all nonkernel items
       * have their dots at the left side *)
      Some (DottedProduction.symbol_before_dot (List.find (fun item -> item.dprod.dot <> 0) item_set.kernel_items).dprod)
    with Not_found ->
      None


let production_closure env finished worklist item b prod =
  let open GrammarType in
  if Config.trace_closure then (
    print_string "    considering production ";
    PrintGrammar.print_production prod;
    print_newline ();
  );

  (* key to good performance: do *no* dynamic allocation in this
   * loop (one of two inner loops in the grammar analysis), until a
   * new item is actually *needed* (which is the uncommon case) *)

  (* invariant of the indexed productions list *)
  assert (prod.left == b);

  (* construct "B -> . gamma, First(beta LA)";
   * except, don't actually build it until later; in the meantime,
   * determine which DP and lookahead it would use if created *)
  let new_dp = DottedProduction.get env.dotted_prods prod 0 (* dot at left *) in

  (* get beta (what follows B in 'item') *)
  let beta = DottedProduction.next env.dotted_prods item.dprod in

  (* get First(beta) -> new item's lookahead *)
  let new_item_la = TerminalSet.copy beta.first_set in

  (* if beta ->* epsilon, add LA *)
  if beta.can_derive_empty then (
    if Config.trace_closure then (
      print_string "      beta: ";
      PrintAnalysisEnv.print_dotted_production beta;
      print_newline ();
      print_endline "      beta can derive empty";
    );
    TerminalSet.unite new_item_la item.lookahead;
  ) else (
    if Config.trace_closure then (
      print_endline "      beta can NOT derive empty";
    );
  );

  (* except we do not want to put terminals in the lookahead set
   * for which 'prod' is not allowed to reduce when they are next *)
  TerminalSet.differentiate new_item_la prod.forbid;

  if Config.trace_closure then (
    print_string "      built item ";
    PrintAnalysisEnv.print_lr_item env { dprod = new_dp; lookahead = new_item_la };
    print_newline ();
  );

  (* is 'newDP' already there?
   * check in working and finished tables *)
  let in_done_list, already =
    match new_dp.back_pointer with
    | Some _ as item ->
        false, item
    | None ->
        let item =
          try
            Some (DottedProductionTable.find finished new_dp)
          with Not_found ->
            None
        in
        true, item
  in

  match already with
  | Some already ->
      (* yes, it's already there *)
      if Config.trace_closure then (
        print_string "      looks similar to ";
        PrintAnalysisEnv.print_lr_item env already;
        print_newline ();
      );

      (* but the new item may have additional lookahead
       * components, so merge them with the old *)
      if TerminalSet.merge already.lookahead new_item_la then (
        if Config.trace_closure then (
          print_string "      (chg) merged it to make ";
          PrintAnalysisEnv.print_lr_item env already;
          print_newline ();
        );

        (* merging changed 'already' *)
        if in_done_list then (
          (* pull from the 'done' list and put in worklist, since the
           * lookahead changed *)
          DottedProductionTable.remove finished already.dprod;
          assert (already.dprod.back_pointer == None); (* was not on the worklist *)
          Stack.push already worklist;
          already.dprod.back_pointer <- Some already; (* now is on worklist *)
        ) else (
          (* else, 'already' is in the worklist, so that's fine *)
        )
      ) else (
        (* else, the dprod already existed *)
      )

  | None ->
      (* it's not already there, so add it to worklist (but first
       * actually create it!) *)
      let new_item = {
        dprod = new_dp;
        lookahead = new_item_la;
      } in

      if Config.trace_closure then (
        print_endline "      this dprod is new, queueing it to add";
      );

      Stack.push new_item worklist;
      assert (new_item.dprod.back_pointer == None);
      new_item.dprod.back_pointer <- Some new_item


let single_item_closure env finished worklist item =
  let open GrammarType in
  let open AnalysisEnvType in
  (* in comments that follow, 'item' is broken down as
   *   A -> alpha . B beta, LA *)

  if Config.trace_closure then (
    print_string "%%% considering item ";
    PrintAnalysisEnv.print_lr_item env item;
    print_newline ();
  );

  (* get the symbol B (the one right after the dot) *)
  match LrItem.symbol_after_dot item with
  | None ->
      (* dot is at the end *)
      if Config.trace_closure then (
        print_endline "    dot is at the end"
      )

  | Some (Terminal _) ->
      (* symbol after the dot is a terminal *)
      if Config.trace_closure then (
        print_endline "    symbol after the dot is a terminal"
      )

  | Some (Nonterminal (_, b)) ->
      (* for each production "B -> gamma" *)
      List.iter (production_closure env finished worklist item b) env.prods_by_lhs.(b.nt_index)


(* based on [ASU] figure 4.33, p.223
 * NOTE: sometimes this is called with nonempty nonkernel items... *)
let item_set_closure env item_set =
  (* every 'item' on the worklist has item.dprod.back_pointer = Some item;
   * every 'dprod' not associated has dprod.back_pointer = None *)
  let worklist = Stack.create () in

  if Config.trace_closure then (
    print_string "%%% computing closure of ";
    PrintAnalysisEnv.print_item_set env.env item_set;
  );

  (* set of items we've finished *)
  let finished = DottedProductionTable.create 13 in

  (* put all the nonkernels we have into 'finished' *)
  List.iter (fun item ->
    DottedProductionTable.add finished item.dprod item
  ) item_set.nonkernel_items;

  (* clear the non-kernel item list *)
  item_set.nonkernel_items <- [];

  (* first, close the kernel items -> worklist *)
  List.iter (fun item ->
    single_item_closure env.env finished worklist item
  ) item_set.kernel_items;

  while not (Stack.is_empty worklist) do
    (* pull the first production *)
    let item = Stack.pop worklist in
    assert (DottedProduction.back_pointer item.dprod == item); (* was on worklist *)
    item.dprod.back_pointer <- None; (* now off worklist *)

    (* put it into list of 'done' items; this way, if this
     * exact item is generated during closure, it will be
     * seen and re-inserted (instead of duplicated) *)
    DottedProductionTable.add finished item.dprod item;

    (* close it -> worklist *)
    single_item_closure env.env finished worklist item
  done;

  (* move everything from 'finished' to the nonkernel items list *)
  item_set.nonkernel_items <-
    DottedProductionTable.fold (fun dprod item items ->
      item :: items
    ) finished [];

  (* we potentially added a bunch of things *)
  changed_items item_set;

  if Config.trace_closure then (
    print_string "%%% done with closure of ";
    PrintAnalysisEnv.print_item_set env.env item_set;
  )


(* yield a new kernel item list by moving the dot across the productions
 * in 'source' that have 'symbol' to the right of the dot; do *not*
 * compute the closure *)
let move_dot_no_closure nonterm_count term_count dotted_prods source symbol =
  let kernel_items =
    ListUtil.fold_left_many (fun kernel_items item ->
      match LrItem.symbol_after_dot item with
      (* dot is already at end *)
      | None -> kernel_items
      (* can't move dot *)
      | Some sym when not (GrammarUtil.equal_symbol sym symbol) -> kernel_items

      | Some _ ->
          let dot_moved = {
            (* move the dot; write dot-moved item into 'dot_moved' *)
            dprod = DottedProduction.next dotted_prods item.dprod;
            lookahead = TerminalSet.copy item.lookahead;
          } in

          dot_moved :: kernel_items
    ) [] [source.kernel_items; source.nonkernel_items]
  in

  assert (kernel_items <> []);

  (* we added stuff; sorting is needed both for hashing, and also
   * for the lookahead merge step that follows a successful lookup *)
  let kernel_items = List.sort LrItemS.compare kernel_items in
  {
    kernel_items;
    nonkernel_items = [];
    term_transition = [||];
    nonterm_transition = [||];
    dots_at_end = [];
    state_symbol = None;
    state_id = state_id_of_int (-1);
    bfs_parent = None;
    hash = ItemSetS.hash_kernel_items kernel_items;
  }
  


let merge_lookaheads_into dest items =
  let changed = ref false in

  (* iterate over both kernel lists simultaneously *)
  List.iter2 (fun dest_item source_item ->
    (* the caller should already have established equality of the
     * non-lookahead components of the kernel items *)
    assert (dest_item.dprod == source_item.dprod);

    if TerminalSet.merge dest_item.lookahead source_item.lookahead then
      changed := true
  ) dest.kernel_items items;

  !changed


let merge_or_create_state env item_set sym in_done_list already with_dot_moved =
  match already with
  | Some already ->
      assert (List.length with_dot_moved.kernel_items = List.length already.kernel_items);

      (* we already have a state with at least equal kernel items, not
       * considering their lookahead sets; so we have to merge the
       * computed lookaheads with those in 'already' *)
      if merge_lookaheads_into already with_dot_moved.kernel_items then (
        if Config.trace_lrsets then (
          Printf.printf "from state %d, found that the transition on %s yielded a state similar to %d, but with different lookahead\n"
            (int_of_state_id item_set.state_id)
            (GrammarUtil.name_of_symbol sym)
            (int_of_state_id already.state_id)
        );

        (* this changed 'already'; recompute its closure *)
        item_set_closure env already;

        (* and reconsider all of the states reachable from it *)
        if not in_done_list then (
          (* item_sets_pending contains 'already', it will be processed later *)
        ) else (
          (* we thought we were done with this *)
          assert (ItemSetMap.mem already env.item_sets_done);

          (* but we're not: move it back to the 'pending' list *)
          env.item_sets_done <- ItemSetMap.remove already env.item_sets_done;
          ItemSetStack.push env.item_sets_pending already;
        )
      );

      (* use existing one for setting the transition function *)
      already

  | None ->
      (* we don't already have it; need to actually allocate & copy *)
      let with_dot_moved = make_item_set ~hash:with_dot_moved.hash env with_dot_moved.kernel_items in

      (* finish it by computing its closure *)
      item_set_closure env with_dot_moved;

      (* then add it to 'pending' *)
      ItemSetStack.push env.item_sets_pending with_dot_moved;

      with_dot_moved


let create_transition env item_set sym =
  (* compute the item_set produced by moving the dot across 'sym';
   * don't take closure yet since we first want to check whether it
   * is already present
   *
   * this call also yields the unused remainder of the kernel items,
   * so we can add them back in at the end *)
  let with_dot_moved =
    move_dot_no_closure env.nonterm_count env.term_count env.env.dotted_prods item_set sym
  in

  (* see if we already have it, in either set *)
  let in_done_list, already =
    try
      false, Some (ItemSetStack.find env.item_sets_pending with_dot_moved)
    with Not_found ->
      true, ItemSetMap.Exceptionless.find with_dot_moved env.item_sets_done
  in

  if Config.trace_lrsets then (
    print_endline (if in_done_list then "from done list" else "from pending list");
  );

  let with_dot_moved =
    merge_or_create_state env item_set sym in_done_list already with_dot_moved
  in

  (* setup the transition function *)
  ItemSet.set_transition item_set sym with_dot_moved


(* for each production in the item set where the
 * dot is not at the right end *)
let process_item env item_set item =
  match LrItem.symbol_after_dot item with
  | None ->
      (* dot is at the end *)
      ()

  | Some sym ->
      if Config.trace_lrsets then (
        print_string "%%% considering item ";
        PrintAnalysisEnv.print_lr_item env.env item;
        print_newline ();
      );

      (* in LALR(1), two items might have different lookaheads; more
       * likely, re-expansions needs to propagate lookahead that
       * wasn't present from an earlier expansion
       *
       * if we already have a transition for this symbol,
       * there's nothing more to be done *)
      if Config.use_LALR1 || ItemSet.transition item_set sym == None then
        create_transition env item_set sym


let process_item_set env =
  let item_set = ItemSetStack.pop env.item_sets_pending in

  (* put it in the done set; note that we must do this *before*
   * the processing below, to properly handle self-loops *)
  env.item_sets_done <- ItemSetMap.add item_set item_set env.item_sets_done;

  if Config.trace_lrsets then (
    print_string "%%% ";
    Printf.printf "state %d, %d kernel items and %d nonkernel items\n"
      (int_of_state_id item_set.state_id)
      (List.length item_set.kernel_items)
      (List.length item_set.nonkernel_items)
  );

  List.iter (process_item env item_set) item_set.kernel_items;
  List.iter (process_item env item_set) item_set.nonkernel_items


let construct_lr_item_sets env =
  let open GrammarType in

  let env = {
    env = env;

    nonterm_count = Array.length env.indexed_nonterms;
    term_count    = Array.length env.indexed_terms;

    next_item_set_id = 0;

    item_sets_pending = ItemSetStack.create 13;
    item_sets_done = ItemSetMap.empty;
  } in

  (* start by constructing closure of first production
   * (basically assumes first production has start symbol
   * on LHS, and no other productions have the start symbol
   * on LHS) *)
  begin
    let first_dp = DottedProduction.get env.env.dotted_prods env.env.indexed_prods.(0) 0 (* dot at left *) in
    assert (first_dp.prod == env.env.indexed_prods.(0));
    assert (first_dp.prod.left.nbase.name == GrammarTreeParser.start_name);

    let first_item = {
      dprod = first_dp;
      lookahead = TerminalSet.create env.term_count;
    } in

    let item_set = make_item_set env [first_item] in
    env.env.start_state <- Some item_set;

    (* EOF is not added to the lookahead; we assume EOF is actually
     * mentioned in the production already, and we won't contemplate
     * executing this reduction within the normal parser core
     * (see Glr.cleanupAfterParse) *)

    item_set_closure env item_set;

    (* this makes the initial pending item_set *)
    ItemSetStack.push env.item_sets_pending item_set
  end;

  (* for each pending item set *)
  while not (ItemSetStack.is_empty env.item_sets_pending) do
    process_item_set env
  done;

  if true || Config.trace_lrsets then (
    print_string "%%% ";
    Printf.printf "finished item set construction with %d states\n" (ItemSetMap.cardinal env.item_sets_done);
  );

  let states =
    ItemSetMap.fold (fun item_set _ ids -> item_set :: ids) env.item_sets_done []
    |> List.sort (fun a b -> compare a.state_id b.state_id)
  in

  states
