let first_of_sequence derivable seq =
  let open Gramtype in
  let dest = TerminalSet.create 160 in

  (* for each sequence member such that all
   * preceeding members can derive the empty string *)
  ignore (ListUtil.iter_until (fun sym ->
    match sym with
    (* LHS -> x alpha   means x is in First(LHS) *)
    | Terminal (_, term) ->
        TerminalSet.set dest term.term_index;

        (* stop considering RHS members since a terminal
         * effectively "hides" all further symbols from First *)
        true

    | Nonterminal (_, nonterm) ->
        (* anything already in nonterm's First should be added to dest *)
        TerminalSet.unite dest nonterm.first;

        (* if nt can't derive the empty string, then it blocks further
         * consideration of right-hand side members *)
        not (Derivability.can_derive_empty derivable nonterm)
  ) seq);

  dest


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
let compute_first derivable indexed_nonterms indexed_prods =
  let open Gramtype in
  let changed = ref true in

  while !changed do
    changed := false;

    (* for each production *)
    Array.iter (fun prod ->
      let lhs = prod.left in

      (* compute First(RHS-sequence) *)
      let first_of_rhs = first_of_sequence derivable prod.right in

      (* check whether First(LHS) will change by uniting it with Firs(RHS-sequence) *)
      let merged = TerminalSet.union lhs.first first_of_rhs in
      if not (TerminalSet.equals lhs.first merged) then
        changed := true;
      (* add everything in First(RHS-sequence) to First(LHS) *)
      TerminalSet.unite lhs.first first_of_rhs
    ) indexed_prods

  done


let compute_dprod_first derivable dotted_prods indexed_prods =
  let open Gramtype in
  let open AnalysisEnvType in

  let empty = TerminalSet.empty () in

  (* for each production *)
  Array.iter (fun prod ->
    (* for each dotted production where the dot is not at the end.. *)
    let rhs_length = List.length prod.right in
    for posn = 0 to rhs_length do
      let dprod = dotted_prods.(prod.prod_index).(posn) in

      (* compute its first *)
      TerminalSet.intersect dprod.first_set empty;
      let first_of_rhs = first_of_sequence derivable dprod.prod.right in

      (* can it derive empty? *)
      dprod.can_derive_empty <- Derivability.can_sequence_derive_empty derivable dprod.prod.right
    done
  ) indexed_prods
