let first_of_sequence derivable seq =
  (* for each sequence member such that all
   * preceding members can derive the empty string *)
  fst (List.fold_left (fun (dest, blocked) sym ->
    let open GrammarType in
    if blocked then
      dest, true
    else
      match sym with
      (* LHS -> x alpha   means x is in First(LHS) *)
      | Terminal (_, term) ->
          let dest = TerminalSet.add term.term_index dest in

          (* stop considering RHS members since a terminal
           * effectively "hides" all further symbols from First *)
          dest, true

      | Nonterminal (_, nonterm) ->
          (* anything already in nonterm's First should be added to dest *)
          let dest = TerminalSet.union nonterm.first dest in

          (* if nonterm can't derive the empty string, then it blocks
           * further consideration of right-hand side members *)
          dest, not (Derivability.can_derive_empty derivable nonterm)
  ) (TerminalSet.empty, false) seq)


(* Compute, for each nonterminal, the "First" set, defined as:
 *
 *   First(N) = { x | N ->* x alpha }, where alpha is any sequence
 *                                     of terminals and nonterminals
 *
 * If N can derive the empty string, I'm going to say that empty is
 * *not* in First, despite what Aho/Sethi/Ullman says.  I do this
 * because I have that information readily as my derivable relation,
 * and because it violates the type system I've devised.
 *
 * I also don't "compute" First for terminals, since they are trivial
 * (First(x) = {x}). *)
let rec compute_first derivable index =
  let open AnalysisEnvType in
  let open GrammarType in

  (* for each production *)
  let changed =
    ProdArray.fold_left (fun changed prod ->
      let lhs = NtArray.get index.nonterms prod.left in

      (* compute First(RHS-sequence) *)
      let first_of_rhs = first_of_sequence derivable prod.right in

      (* add everything in First(RHS-sequence) to First(LHS) *)
      let merged = TerminalSet.union lhs.first first_of_rhs in
      if TerminalSet.equal lhs.first merged then
        changed
      else (
        lhs.first <- merged;

        if Options._trace_first () then (
          print_string "added ";
          PrintAnalysisEnv.print_terminal_set index.terms first_of_rhs;
          print_string " to ";
          print_string lhs.nbase.name;
          print_string " because of ";
          PrintGrammar.print_production index.nonterms prod;
          print_newline ();
        );

        true
      )
    ) false index.prods
  in

  if changed then
    (* loop until no changes *)
    compute_first derivable index
  else (
    if Options._trace_first () then (
      NtArray.iter (fun nonterm ->
        if nonterm != empty_nonterminal then (
          PrintAnalysisEnv.print_terminal_set ~name:nonterm.nbase.name index.terms nonterm.first;
          print_newline ();
        )
      ) index.nonterms
    )
  )


let compute_dprod_first derivable dotted_prods index =
  let open GrammarType in
  let open AnalysisEnvType in

  (* for each production *)
  ProdArray.iteri (fun prod_index prod ->
    let dprods = ProdArray.get dotted_prods prod_index in

    (* for each dotted production where the dot is not at the end.. *)
    let rhs_length = List.length prod.right in
    for posn = 0 to rhs_length do
      let dprod = dprods.(posn) in

      let right = ExtList.nth_tl dprod.prod.right posn in

      (* compute its first *)
      let first_of_rhs = first_of_sequence derivable right in
      dprod.first_set <- first_of_rhs;

      (* can it derive empty? *)
      dprod.can_derive_empty <- Derivability.can_sequence_derive_empty derivable right;

      if Options._trace_first () then (
        PrintAnalysisEnv.print_dotted_production ~terms:index.terms index.nonterms dprod;

        if dprod.can_derive_empty then
          print_endline " - can derive empty"
        else
          print_endline " - can NOT derive empty"
      )
    done

  ) index.prods
