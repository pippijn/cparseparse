let compute_follow derivable indexed_prods =
  let open Gramtype in
  let changed = ref true in

  (* loop until no changes *)
  while !changed do
    changed := false;

    (* for each production *)
    Array.iter (fun prod ->
      (* for each RHS nonterminal member *)
      ListUtil.iterl (fun after_right_sym right_sym ->
        match right_sym with
        | Terminal _ -> ()
        | Nonterminal (_, right_nonterm) ->
            (* I'm not sure what it means to compute Follow(emptyString),
             * so let's just not do so *)
            assert (right_nonterm != empty_nonterminal);

            (* rule 1:
             * if there is a production A -> alpha B beta, then
             * everything in First(beta) is in Follow(B) *)

            (* compute First(beta) *)
            let first_of_beta = FirstSets.first_of_sequence derivable after_right_sym in

            (* put those into Follow(right_nonterm) *)
            if TerminalSet.merge right_nonterm.follow first_of_beta then
              changed := true;

            (* rule 2:
             * if there is a production A -> alpha B, or a
             * production A -> alpha B beta where beta ->* empty ... *)
            if Derivability.can_sequence_derive_empty derivable after_right_sym then
              (* ... then everything in Follow(A) is in Follow(B) *)
              if TerminalSet.merge right_nonterm.follow prod.left.follow then
                changed := true;
      ) prod.right;

    ) indexed_prods

  done
