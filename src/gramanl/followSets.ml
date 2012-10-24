let compute_follow derivable index =
  let open AnalysisEnvType in
  let open GrammarType in
  let changed = ref true in

  (* loop until no changes *)
  while !changed do
    changed := false;

    (* for each production *)
    ProdArray.iter (fun prod ->
      (* for each RHS nonterminal member *)
      ExtList.iterl (fun after_right_sym right_sym ->
        match right_sym with
        | Terminal _ -> ()
        | Nonterminal (_, right_nonterm) ->
            (* RHS should never contain the empty string. Also, I'm not sure
             * what it means to compute Follow(empty), so let's just not do so *)
            assert (right_nonterm != empty_nonterminal);

            begin
              (* rule 1:
               * if there is a production A -> alpha B beta, then
               * everything in First(beta) is in Follow(B) *)

              (* compute First(beta) *)
              let first_of_beta = FirstSets.first_of_sequence derivable after_right_sym in

              (* put those into Follow(right_nonterm) *)
              let merged = TerminalSet.union right_nonterm.follow first_of_beta in
              if not (TerminalSet.equal right_nonterm.follow merged) then (
                right_nonterm.follow <- merged;
                changed := true;
              )
            end;

            begin
              (* rule 2:
               * if there is a production A -> alpha B, or a
               * production A -> alpha B beta where beta ->* empty ... *)
              if Derivability.can_sequence_derive_empty derivable after_right_sym then
                (* ... then everything in Follow(A) is in Follow(B) *)
                let left = NtArray.get index.nonterms prod.left in
                let merged = TerminalSet.union right_nonterm.follow left.follow in
                if not (TerminalSet.equal right_nonterm.follow merged) then (
                  right_nonterm.follow <- merged;
                  changed := true;
                )
            end;

      ) prod.right;

    ) index.prods

  done
