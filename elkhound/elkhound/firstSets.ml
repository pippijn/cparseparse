let first_of_sequence derivable seq term_count =
  (* for each sequence member such that all
   * preceeding members can derive the empty string *)
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
let compute_first derivable indexed_nonterms indexed_prods indexed_terms =
  let open GrammarType in
  let term_count = Array.length indexed_terms in
  let changed = ref true in

  while !changed do
    changed := false;

    (* for each production *)
    Array.iter (fun prod ->
      let lhs = prod.left in

      (* compute First(RHS-sequence) *)
      let first_of_rhs = first_of_sequence derivable prod.right term_count in

      (* add everything in First(RHS-sequence) to First(LHS) *)
      let merged = TerminalSet.union lhs.first first_of_rhs in
      if not (TerminalSet.equal lhs.first merged) then (
        lhs.first <- merged;
        changed := true;

        if Config.trace_first then (
          print_string "added ";
          PrintAnalysisEnv.print_terminal_set indexed_terms first_of_rhs;
          print_string " to ";
          print_string lhs.nbase.name;
          print_string " because of ";
          PrintGrammar.print_production prod;
          print_newline ();
        );
      )
    ) indexed_prods

  done;

  if Config.trace_first then (
    Array.iter (fun nonterm ->
      if nonterm != empty_nonterminal then (
        PrintAnalysisEnv.print_terminal_set ~name:nonterm.nbase.name indexed_terms nonterm.first;
        print_newline ();
      )
    ) indexed_nonterms
  )


let compute_dprod_first derivable dotted_prods indexed_prods indexed_terms =
  let open GrammarType in
  let open AnalysisEnvType in
  let term_count = Array.length indexed_terms in

  (* for each production *)
  Array.iter (fun prod ->

    (* for each dotted production where the dot is not at the end.. *)
    let rhs_length = List.length prod.right in
    for posn = 0 to rhs_length do
      let dprod = dotted_prods.(prod.prod_index).(posn) in

      let right = ListUtil.nth_tl dprod.prod.right posn in

      (* compute its first *)
      let first_of_rhs = first_of_sequence derivable right term_count in
      dprod.first_set <- first_of_rhs;

      (* can it derive empty? *)
      dprod.can_derive_empty <- Derivability.can_sequence_derive_empty derivable right;

      if Config.trace_first then (
        PrintAnalysisEnv.print_dotted_production ~terms:indexed_terms dprod;

        if dprod.can_derive_empty then
          print_endline " - can derive empty"
        else
          print_endline " - can NOT derive empty"
      )
    done

  ) indexed_prods
