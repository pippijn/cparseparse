open Batteries_uni
open AnalysisEnvType

let tr = true


let use_LR0 = false
let use_SLR1 = false
let use_LR1 = false
let use_LALR1 = true


let push stack item =
  stack := item :: !stack

let pop stack =
  let item = List.hd !stack in
  stack := List.tl !stack;
  item


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
      (List.find (fun item -> item.dprod.dot <> 0) item_set.kernel_items).dprod.before_dot
    with Not_found ->
      None



let single_item_closure prods_by_lhs dotted_prods finished worklist item term_count =
  let open GrammarType in
  let open AnalysisEnvType in
  (* in comments that follow, 'item' is broken down as
   *   A -> alpha . B beta, LA *)

  (* get the symbol B (the one right after the dot) *)
  match item.dprod.after_dot with
  | None ->
      (* dot is at the end *)
      ()

  | Some (Terminal _) ->
      (* symbol after the dot is a terminal *)
      ()

  | Some (Nonterminal (_, b)) ->
      (* for each production "B -> gamma" *)
      List.iter (fun prod ->
        (* key to good performance: do *no* dynamic allocation in this
         * loop (one of two inner loops in the grammar analysis), until a
         * new item is actually *needed* (which is the uncommon case) *)

        (* invariant of the indexed productions list *)
        assert (prod.left == b);

        (* construct "B -> . gamma, First(beta LA)";
         * except, don't actually build it until later; in the meantime,
         * determine which DP and lookahead it would use if created *)
        let new_dp = DottedProduction.get dotted_prods prod 0 (* dot at left *) in

        (* get beta (what follows B in 'item') *)
        let beta = DottedProduction.next dotted_prods item.dprod in

        (* get First(beta) -> new item's lookahead *)
        let new_item_la = TerminalSet.copy beta.first_set in

        (* if beta ->* epsilon, add LA *)
        if beta.can_derive_empty then
          TerminalSet.unite new_item_la item.lookahead;

        (* except we do not want to put terminals in the lookahead set
         * for which 'prod' is not allowed to reduce when they are next *)
        TerminalSet.differentiate new_item_la prod.forbid;

        (* is 'newDP' already there?
         * check in working and finished tables *)
        let in_done_list, already =
          match new_dp.back_pointer with
          | Some _ as item ->
              false, item
          | None ->
              let item =
                try
                  Some (Hashtbl.find finished new_dp)
                with Not_found ->
                  None
              in
              true, item
        in

        match already with
        | Some already ->
            (* yes, it's already there,
             * but the new item may have additional lookahead
             * components, so merge them with the old *)
            if TerminalSet.merge already.lookahead new_item_la then
              (* merging changed 'already' *)
              if in_done_list then (
                Hashtbl.remove finished already.dprod;
                push worklist already;
                assert (already.dprod.back_pointer == None); (* was not on the worklist *)
                already.dprod.back_pointer <- Some already;
              ) (* else, 'already' is in the worklist, so that's fine *)
            (* else, the dprod already existed *)

        | None ->
            (* it's not already there, so add it to worklist (but first
             * actually create it!) *)
            let new_item = {
              dprod = new_dp;
              lookahead = new_item_la;
            } in

            push worklist new_item;
            assert (new_item.dprod.back_pointer == None);
            new_item.dprod.back_pointer <- Some new_item

      ) prods_by_lhs.(b.nt_index);

      ()


(* based on [ASU] figure 4.33, p.223
 * NOTE: sometimes this is called with nonempty nonkernel items... *)
let item_set_closure prods_by_lhs dotted_prods item_set term_count =
  (* every 'item' on the worklist has item.dprod.back_pointer = Some item;
   * every 'dprod' not associated has dprod.back_pointer = None *)
  let worklist = ref [] in

  if tr then (
    print_string "computing closure of ";
    PrintAnalysisEnv.print_item_set item_set;
    raise Exit
  );

  (* set of items we've finished *)
  let finished = Hashtbl.create 13 in

  (* put all the nonkernels we have into 'finished' *)
  List.iter (fun item ->
    Hashtbl.add finished item.dprod item
  ) item_set.nonkernel_items;

  (* first, close the kernel items -> worklist *)
  List.iter (fun item ->
    single_item_closure prods_by_lhs dotted_prods finished worklist item term_count
  ) item_set.kernel_items;

  while !worklist <> [] do
    (* pull the first production *)
    let item = pop worklist in
    assert (item.dprod.back_pointer = Some item); (* was on worklist *)
    item.dprod.back_pointer <- None; (* now off worklist *)

    (* put it into list of 'done' items; this way, if this
     * exact item is generated during closure, it will be
     * seen and re-inserted (instead of duplicated) *)
    Hashtbl.add finished item.dprod item;

    (* close it -> worklist *)
    single_item_closure prods_by_lhs dotted_prods finished worklist item term_count
  done;

  (* move everything from 'finished' to the nonkernel items list *)
  Hashtbl.iter (fun dprod item ->
    item_set.nonkernel_items <- item :: item_set.nonkernel_items
  ) finished;

  changed_items item_set


(* yield a new kernel item list by moving the dot across the productions
 * in 'source' that have 'symbol' to the right of the dot; do *not*
 * compute the closure *)
let move_dot_no_closure dotted_prods source symbol term_count =
  let kernel_items =
    ListUtil.fold_left_many (fun kernel_items item ->
      if LrItem.is_dot_at_end item || item.dprod.after_dot <> Some symbol then (
        kernel_items (* can't move dot *)
      ) else (

        let dot_moved = {
          (* move the dot; write dot-moved item into 'dot_moved' *)
          dprod = DottedProduction.next dotted_prods item.dprod;
          lookahead = TerminalSet.copy item.lookahead;
        } in

        dot_moved :: kernel_items
      )
    ) [] [source.kernel_items; source.nonkernel_items]
  in

  assert (kernel_items <> []);

  (* we added stuff; sorting is needed both for hashing, and also
   * for the lookahead merge step that follows a successful lookup *)
  List.sort ~cmp:LrItemS.compare kernel_items


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


let construct_lr_item_sets env =
  let open GrammarType in

  let nonterm_count = Array.length env.indexed_nonterms in
  let term_count    = Array.length env.indexed_terms in

  let next_item_set_id = ref 0 in

  let make_item_set kernel_items =
    let state_id = Obj.magic !next_item_set_id in
    incr next_item_set_id;

    {
      kernel_items = kernel_items;
      nonkernel_items = [];
      term_transition = Array.make term_count None;
      nonterm_transition = Array.make nonterm_count None;
      dots_at_end = [];
      state_symbol = None;
      state_id = state_id;
      bfs_parent = None;
    }
  in

  let item_sets_pending = ref ItemSetSet.empty in
  let item_sets_done = ref [] in

  (* start by constructing closure of first production
   * (basically assumes first production has start symbol
   * on LHS, and no other productions have the start symbol
   * on LHS) *)
  begin
    let first_dp = DottedProduction.get env.dotted_prods env.indexed_prods.(0) 0 (* dot at left *) in
    assert (first_dp.prod == env.indexed_prods.(0));
    assert (first_dp.prod.left.nbase.name == Grammar.start_name);

    PrintAnalysisEnv.print_dotted_production first_dp;

    let first_item = {
      dprod = first_dp;
      lookahead = TerminalSet.create term_count;
    } in

    let item_set = make_item_set [] in
    env.start_state <- Some item_set;

    item_set.kernel_items <- [first_item];

    (* EOF is not added to the lookahead; we assume EOF is actually
     * mentioned in the production already, and we won't contemplate
     * executing this reduction within the normal parser core
     * (see Glr.cleanupAfterParse) *)

    item_set_closure env.prods_by_lhs env.dotted_prods item_set term_count;

    (* this makes the initial pending item_set *)
    item_sets_pending := ItemSetSet.add item_set !item_sets_pending;
  end;

  Sexplib.Sexp.output_hum Pervasives.stdout (ItemSetSet.sexp_of_t !item_sets_pending);

  (*
  (* for each pending item set *)
  while !item_sets_pending <> [] do
    let item_set = pop item_sets_pending in

    (* put it in the done set; note that we must do this *before*
     * the processing below, to properly handle self-loops *)
    push item_sets_done item_set;

    (* see below; this is part of a fix for a *very* subtle heisenbug *)
    let must_close_myself = ref false in

    (* for each production in the item set where the
     * dot is not at the right end *)
    List.iter (fun item ->
      begin match item.dprod.after_dot with
      | None ->
          (* dot is at the end *)
          ()

      | Some sym ->
          (* in LALR(1), two items might have different lookaheads; more
           * likely, re-expansions needs to propagate lookahead that
           * wasn't present from an earlier expansion
           *
           * if we already have a transition for this symbol,
           * there's nothing more to be done *)
          if use_LALR1 || transition item_set sym == None then (
            (* compute the item_set produced by moving the dot across 'sym';
             * don't take closure yet since we first want to check whether it
             * is already present
             *
             * this call also yields the unused remainder of the kernel items,
             * so we can add them back in at the end *)
            let kernel_items =
              move_dot_no_closure env.dotted_prods item_set sym term_count
            in

            (* see if we already have it, in either set *)
            let in_done_list, already =
              let predicate item_set = item_set.kernel_items = kernel_items in
              try
                false, Some (List.find predicate !item_sets_pending)
              with Not_found ->
                true, List.Exceptionless.find predicate !item_sets_done
            in

            let with_dot_moved =
              match already with
              | Some already ->
                  (* we already have a state with at least equal kernel items, not
                   * considering their lookahead sets; so we have to merge the
                   * computed lookaheads with those in 'already' *)
                  if merge_lookaheads_into already kernel_items then (
                    (* this changed 'already'; recompute its closure *)
                    if already != item_set then
                      item_set_closure env.prods_by_lhs env.dotted_prods already term_count
                    else
                      (* DANGER!  I'm already iterating over 'itemSet's item lists,
                       * and if I execute the closure algorithm it will invalidate
                       * my iterator.  so, postpone it *)
                      must_close_myself := true;

                    (* and reconsider all of the states reachable from it *)
                    if not in_done_list then (
                      (* item_sets_pending contains 'already', it will be processed later *)
                    ) else (
                      (* we thought we were done with this *)
                      (*assert (contains item_sets_done already);*)

                      (* but we're not: move it back to the 'pending' list *)
                      remove item_sets_done already;
                      push item_sets_pending already;
                    )
                  );

                  (* use existing one for setting the transition function *)
                  already

              | None ->
                  (* we don't already have it; need to actually allocate & copy *)
                  let with_dot_moved = make_item_set kernel_items in

                  (* finish it by computing its closure *)
                  item_set_closure env.prods_by_lhs env.dotted_prods with_dot_moved term_count;

                  (* then add it to 'pending' *)
                  push item_sets_pending with_dot_moved;

                  with_dot_moved
            in

            (* setup the transition function *)
            set_transition item_set sym with_dot_moved;

            ()
          )
      end;

      (* now that we're finished iterating over the items, I can do the
       * postponed closure *)
      if !must_close_myself then
        item_set_closure env.prods_by_lhs env.dotted_prods item_set term_count;

    ) item_set.kernel_items;
  done;
  *)

  ()
