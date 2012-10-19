open AnalysisEnvType
open GrammarType


let can_derive_i derivable left right =
  Bit2d.is_set derivable left right

let can_derive derivable left right =
  can_derive_i derivable left.nt_index right.nt_index

let can_derive_empty derivable nonterm =
  can_derive derivable nonterm empty_nonterminal

let can_sequence_derive_empty derivable seq =
  (* XXX: double negation is not pretty *)
  not (
    (* look through the sequence; if any members cannot derive
     * the empty string, fail *)
    ListUtil.iter_until (function
      | Terminal _ ->
          true (* terminals can't derive the empty string *)
      | Nonterminal (_, nonterm) ->
          not (can_derive_empty derivable nonterm)
    ) seq
  )


let add_derivable_i env left right =
  (* Almost as an aside, I'd like to track cyclicity in grammars.
   * It's always true that N ->* N, because 0 steps are allowed.
   * A grammar is cyclic if N ->+ N, i.e. it derives itself in
   * 1 or more steps.
   *
   * We can detect that fairly easily by tracking calls to
   * this fn with left==right.  Since N ->* N in 0 steps is
   * recorded during init (and *not* by calling this fn), the
   * only calls to this with left==right will be when the
   * derivability code detects a nonzero-length path. *)
  if left == right then (
    let left = env.indexed_nonterms.(left) in (* == right *)

    if Options._trace_derivable () then (
      if not left.cyclic then (
        print_string "%%% derivable: ";
        Printf.printf "discovered that %s ->+ %s (i.e. is cyclic)\n"
          left.nbase.name
          left.nbase.name;
      )
    );

    left.cyclic <- true; (* => right.cyclic is also true *)
    env.cyclic_grammar <- true; (* for grammar as a whole *)

    (* Even though we didn't know this already, it doesn't
     * constitute a change in the ->* relation (which is what the
     * derivability code cares about), so we do *not* report a
     * change for the cyclicity detection. *)
  );

  (* we only made a change, and hence should return true,
   * if there was a 0 here before *)
  not (Bit2d.test_and_set env.derivable left right)

(* convenience for the function above *)
let add_derivable env left right =
  add_derivable_i env left.nt_index right.nt_index


let initial_derivable_relation nonterm_count =
  (* two-dimensional bit matrix to represent token derivabilities *)
  let derivable = Bit2d.create nonterm_count nonterm_count in

  for i = 0 to nonterm_count - 1 do
    (* every nonterminal can derive itself in 0 or more steps
     * (specifically, in 0 steps, at least) *)
    Bit2d.set derivable i i
  done;

  derivable


let add_derivable_nonterminal env left right_nonterm after_right_sym =
  (* we are wondering if prod.left can derive right_sym.. for
   * this to be true, every symbol that comes after nonterm
   * must be able to derive empty (we've already verified by
   * now that every symbol to the *left* can derive empty) *)
  let rest_derive_empty =
    List.fold_left (fun rest_derive_empty sym ->
      (* if any symbol can't derive empty, then the sequence
       * can't derive empty *)
      rest_derive_empty &&
        match sym with
        | Terminal _ ->
            (* if it's a terminal, it can't derive empty *)
            if Options._trace_derivable () then (
              print_endline "terminal can't derive empty";
            );
            false
        | Nonterminal (_, nonterm) ->
            (* this symbol can't derive empty string (or, we don't
             * yet know that it can), so we conclude that prod.left
             * can't derive right_sym *)
            if Options._trace_derivable () then (
              Printf.printf "nonterminal %s can derive empty?\n" nonterm.nbase.name;
            );
            can_derive_empty env.derivable nonterm
    ) true after_right_sym
  in

  if Options._trace_derivable () then (
    Printf.printf "%s's rest can %sderive empty\n"
      left.nbase.name
      (if rest_derive_empty then "" else "NOT ");
  );

  if rest_derive_empty then (
    (* we have discovered that prod.left can derive right_sym *)
    let added = add_derivable env left right_nonterm in
    assert added; (* above, we verified we didn't already know this *)

    if Options._trace_derivable () then (
      print_string "%%% derivable: ";
      Printf.printf "discovered (by production): %s ->* %s\n"
        left.nbase.name
        right_nonterm.nbase.name;
    );

    true
  ) else (
    false
  )


let add_derivable_relations env changes =
  Array.iter (fun prod ->
    if Options._trace_derivable () then (
      PrintGrammar.print_production prod;
      print_newline ();
    );

    match prod.right with
    | [] ->
        (* since I don't include 'empty' explicitly in my rules, I won't
         * conclude that anything can derive empty, which is a problem;
         * so I special-case it here *)
        ignore (add_derivable env prod.left empty_nonterminal)

    | right ->
        (* iterate over RHS symbols, seeing if the LHS can derive that
         * RHS symbol (by itself) *)
        ignore (ListUtil.fold_leftl (fun derives after_right_sym right_sym ->
          derives &&
            match right_sym with
            | Terminal _ ->
                (* if prod.left derives a string containing a terminal,
                 * then it can't derive any nontermial alone (using this
                 * production, at least) -- empty is considered a nonterminal *)
                false

            | Nonterminal (_, right_nonterm) ->
                (* check if we already know that LHS derives this nonterm *)
                if can_derive env.derivable prod.left right_nonterm then
                  (* we already know that prod.left derives right_sym,
                   * so let's not check it again *)
                  ()
                else if add_derivable_nonterminal env prod.left right_nonterm after_right_sym then
                  incr changes;

                (* ok, we've considered prod.left deriving right_sym.  now, we
                 * want to consider whether prod.left can derive any of the
                 * symbols that follow right_sym in this production.  for this
                 * to be true, right_sym itself must derive the empty string
                 *
                 * if it doesn't -- no point in further consideration of
                 * this production *)
                can_derive_empty env.derivable right_nonterm
        ) true right)

  ) env.indexed_prods


let compute_derivability_closure env changes =
  (* I'll do this by computing R + R^2 -- that is, I'll find all
   * paths of length 2 and add an edge between their endpoints.
   * I do this, rather than computing the entire closure now, since
   * on the next iter I will add more relations and have to re-do
   * a full closure; iterative progress seems a better way.
   *
   * I don't consider edges (u,u) because it messes up my cyclicity
   * detection logic.  (But (u,v) and (v,u) is ok, and in fact is
   * what I want, for detecting cycles.) *)
  let nonterm_count = Array.length env.indexed_nonterms in
  (* for each node u (except empty) *)
  for u = 1 to nonterm_count - 1 do
    (* for each edge (u,v) where u != v *)
    for v = 0 to nonterm_count - 1 do
      if u <> v && can_derive_i env.derivable u v then
        (* for each edge (v,w) where v != w *)
        for w = 0 to nonterm_count - 1 do
          if v <> w && can_derive_i env.derivable v w then
            (* add an edge (u,w), if there isn't one already *)
            if add_derivable_i env u w then (
              if Options._trace_derivable () then (
                print_string "%%% derivable: ";
                Printf.printf "discovered (by closure step): %s ->* %s\n"
                  env.indexed_nonterms.(u).nbase.name
                  env.indexed_nonterms.(w).nbase.name;
              );
              incr changes
            )
        done
    done
  done


let compute_derivability_relation env =
  (* start off with 1 so the loop is entered *)
  let changes = ref 1 in

  (* iterate: propagate 'true' bits across the derivability matrix
   * (i.e. compute transitive closure on the can_derive relation) *)
  while !changes <> 0 do
    changes := 0;

    (* first part: add new can_derive relations *)
    add_derivable_relations env changes;
    let new_relations = !changes in
    if Options._trace_derivable () then
      Printf.printf "%d new relations\n" new_relations;

    (* second part: compute closure over existing relations *)
    compute_derivability_closure env changes;
    if Options._trace_derivable () then
      Printf.printf "%d relations by closure\n" (!changes - new_relations);

  done;

  if Options._trace_derivable () then
    Bit2d.print env.derivable
