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


(* decide what to do, and return the result in the first
 * two tuple members, keep_shift and keep_reduce *)
let handle_shift_reduce_conflict state prod sym =
  let open GrammarType in

  (* look at scannerless directives *)
  let shift_extends_super =
    (* is this nonterm or any of its declared supersets maximal? *)
    let rec loop super maximal =
      match super.superset with
      | Some superset when not maximal ->
          loop superset superset.maximal
      | _ ->
          super, maximal
    in

    let super, maximal = loop prod.left prod.left.maximal in

    (* see if this reduction can be removed due to a 'maximal' spec;
     * in particular, is the shift going to extend 'super'? *)
    maximal && ItemSet.has_extending_shift state super sym
  in

  (* scannerless *)
  if shift_extends_super then
    (* remove reduction due to maximal munch disambiguation *)
    true, false, false

  (* precedence *)
  else if prod.prec = 0 || sym.precedence = 0 then
    (* one of the two doesn't have a precedence specification,
     * so we can do nothing *)
    true, true, false
  else if prod.prec > sym.precedence then
    (* production's precedence is higher, so we choose to reduce
     * instead of shift *)
    false, true, false
  else if prod.prec < sym.precedence then
    (* symbol's precedence is higher, so we shift *)
    true, false, false

  (* precedences are equal, so we look at associativity (of token) *)
  else
    let open Assoc in
    match sym.associativity with
    | AK_LEFT -> false, true, false
    | AK_RIGHT -> true, false, false
    | AK_NONASSOC -> false, false, false
    | AK_NEVERASSOC ->
        (* the user claimed this token would never be involved in a conflict *)
        failwith (Printf.sprintf "token %s was declared 'prec', but it is involved in an associativity conflict with \"%s\" in state %d\n"
          sym.tbase.name
          (* TODO *)"prod"
          (int_of_state_id state.state_id))
    | AK_SPLIT ->
        (* the user does not want disambiguation of this *)
        true, true, true


(* static disambiguation for S/R conflicts *)
let disambiguate_shift_reduce_conflict state sym shift_dest reductions actions suppressed_warnings =
  match shift_dest with
  | Some _ ->
      (* we have (at least) a shift/reduce conflict, which is the
       * situation in which prec/assoc specifications are used; consider
       * all the possible reductions, so we can resolve S/R conflicts
       * even when there are R/R conflicts present too *)
      List.fold_left (fun (shift_dest, reductions) prod ->
        let keep_shift, keep_reduce, dont_warn =
          handle_shift_reduce_conflict state prod sym
        in

        if dont_warn then
          incr suppressed_warnings;

        let shift_dest =
          if keep_shift then (
            shift_dest
          ) else (
            decr actions;
            None
          )
        in

        let reductions =
          if keep_reduce then (
            prod :: reductions
          ) else (
            decr actions;
            reductions
          )
        in

        shift_dest, reductions
      ) (shift_dest, []) reductions

      (* there is still a potential for misbehavior.. e.g., if there are two
       * possible reductions (R1 and R2), and one shift (S), then the user
       * could have specified prec/assoc to disambiguate, e.g.
       *   R1 < S
       *   S < R2
       * so that R2 is the right choice; but if I consider (S,R2) first,
       * I'll simply drop S, leaving no way to disambiguate R1 and R2 ..
       * for now I'll just note the possibility... *)

  | None ->
      shift_dest, reductions


(* the idea is we might be trying to do scannerless parsing, and
 * someone might say that Identifier has as subsets all the keywords,
 * so competing reductions should favor the subsets (the keywords) *)
let subset_directive_resolution state sym reductions actions =
  let open GrammarType in

  (* make a map of which nonterminals appear on the LHS of one
   * of the reductions, and has a superset *)
  let map = BatBitSet.create 0 in
  let any_with_super =
    List.fold_left (fun any prod ->
      if BatOption.is_some prod.left.superset then (
        BatBitSet.set map prod.left.nt_index;
        true
      ) else (
        any
      )
    ) false reductions
  in

  if not any_with_super then (
    reductions (* nothing we can do *)
  ) else (
    (* walk over the reductions, removing those that have reductions
     * to subsets also in the list *)
    List.fold_left (fun reductions prod ->
      let remove =
        List.fold_left (fun remove sub ->
          remove || BatBitSet.is_set map sub.nt_index
        ) false prod.left.subsets
      in
      
      if remove then (
        decr actions;
        reductions
      ) else (
        prod :: reductions
      )
    ) [] reductions
  )


(* given some potential parse actions, apply available disambiguation
 * to remove some of them; print warnings about conflicts, in some
 * situations *)
let resolve_conflicts state (* parse state in which the actions are possible *)
                      sym (* lookahead symbol for these actions *)
                      shift_dest (* (option) the state to which we can shift *)
                      reductions (* list of possible reductions *)
                      allow_ambig (* if false, always return at most 1 action *)
                      sr rr (* counts of S/R and R/R conflicts, resp. *) =
  (* how many actions are there? *)
  let actions = ref ((if BatOption.is_some shift_dest then 1 else 0) + List.length reductions) in
  if !actions <= 1 then (
    (* no conflict *)
    shift_dest, reductions
  ) else (
    (* count how many warning suppressions we have *)
    let suppressed_warnings = ref 0 in

    let shift_dest, reductions =
      disambiguate_shift_reduce_conflict state sym shift_dest reductions actions suppressed_warnings
    in

    (* static disambiguation for R/R conflicts *)
    let reductions =
      if List.length reductions = 1 then (
        reductions
      ) else (
        let open GrammarType in
        (* find the highest precedence *)
        let highest_prec =
          List.fold_left (fun prec prod ->
            max prec prod.prec
          ) 0 reductions
        in

        (* remove any productions that are lower than 'highest_prec' *)
        List.fold_left (fun reductions prod ->
          if prod.prec <> 0 && prod.prec < highest_prec then (
            decr actions;
            reductions
          ) else (
            prod :: reductions
          )
        ) [] reductions
      )
    in

    (* additional R/R resolution using subset directives *)
    let reductions =
      if List.length reductions = 1 then (
        reductions
      ) else (
        subset_directive_resolution state sym reductions actions
      )
    in

    if !actions - !suppressed_warnings <= 1 then (
      (* don't print information about conflicts *)
    ) else (
      if BatOption.is_some shift_dest then (
        incr sr; (* shift/reduce conflict *)
        rr := !rr + !actions - 2 (* any reduces beyond first are r/r errors *)
      ) else (
        rr := !rr + !actions - 1 (* all reduces beyond first are r/r errors *)
      );
    );

    if not allow_ambig && !actions > 1 then (
      (* force only one action, using Bison's disambiguation:
       *   - prefer shift to reduce
       *   - prefer the reduction which occurs first in the grammar file *)
      match shift_dest with
      | Some _ -> shift_dest, []
      | None ->
          None, [
            List.fold_left (fun reduction prod ->
              let open GrammarType in
              (* production indices happen to be assigned in file order *)
              if prod.prod_index > reduction.prod_index then
                prod
              else
                reduction
            ) (List.hd reductions) (List.tl reductions)
          ]
    ) else (
      shift_dest, reductions
    )
  )


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
    (* for each possible lookahead... *)
    Array.iter (fun terminal ->
      (* can shift? *)
      let shift_dest = ItemSet.transition state (GrammarType.Terminal ("", terminal)) in

      (* can reduce? *)
      let reductions = ItemSet.possible_reductions state terminal in

      (* try to resolve conflicts; this may print warnings about
       * the conflicts, depending on various factors; if 'allow_ambig'
       * is false, this will remove all but one action *)
      let shift_dest, reductions =
        resolve_conflicts state terminal shift_dest reductions allow_ambig sr rr
      in

      (* what to do in this cell *)
      (*let actions =*)

      ()
    ) env.indexed_terms;


    ()
  ) states;

  ()
