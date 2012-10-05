open GrammarType
open AnalysisEnvType

let paranoid = false


(* clear first/follow sets by intersecting them with 0 *)
let reset_first_follow prods nonterms term_count =
  let open GrammarType in
  let reset set = TerminalSet.clear set in

  Stringmap.iter (fun _ nonterm ->
    reset nonterm.first;
    reset nonterm.follow;
  ) nonterms;

  List.iter (fun prod ->
    reset prod.first_rhs
  ) prods


let compute_indexed_nonterms nonterms =
  let indexed = Array.make (Stringmap.cardinal nonterms + 1) empty_nonterminal in

  (* indexed.(0) is empty_nonterminal *)
  assert (empty_nonterminal.nt_index = 0);

  let i =
    Stringmap.fold (fun _ nonterm i ->
      nonterm.nt_index <- i; (* map: symbol to index *)
      indexed.(i) <- nonterm; (* map: index to symbol *)
      i + 1
    ) nonterms 1
  in

  assert (i = Array.length indexed);
  (* verify we filled the nt_index map *)
  Array.iter (fun nonterm ->
    assert (nonterm != empty_nonterminal || nonterm.nt_index = 0)
  ) indexed;

  (* number of nonterminals + 1 for empty_nonterminal *)
  assert (Array.length indexed = Stringmap.cardinal nonterms + 1);

  indexed


let compute_indexed_terms terms =
  let indexed = Array.make (Stringmap.cardinal terms) empty_terminal in

  Stringmap.iter (fun _ term ->
    (* the ids have already been assigned *)
    let i = term.term_index in (* map: symbol to index *)
    indexed.(i) <- term (* map: index to symbol *)
  ) terms;

  (* verify we filled the term_index map *)
  Array.iter (fun term -> assert (term != empty_terminal)) indexed;

  assert (Array.length indexed = Stringmap.cardinal terms);

  indexed


let compute_indexed_prods productions nonterm_count =
  (* map: prod_index -> production *)
  let indexed = Array.make (List.length productions) empty_production in
  (* map: nonterminal -> productions with that nonterm on LHS *)
  let prods_by_lhs = Array.make nonterm_count [] in

  let i =
    (* fill in both maps *)
    List.fold_left (fun i production ->
      let nt_index = production.left.nt_index in
      prods_by_lhs.(nt_index) <- production :: prods_by_lhs.(nt_index);
      production.prod_index <- i;
      indexed.(i) <- production;
      i + 1
    ) 0 productions
  in

  assert (i = Array.length indexed);
  (* verify we filled the prod_index map *)
  Array.iter (fun prod -> assert (prod != empty_production)) indexed;

  indexed, prods_by_lhs


let compute_dotted_productions indexed_prods term_count =
  let next_id =
    let next = ref 0 in
    fun () ->
      let id = !next in
      incr next;
      id
  in

  let dotted_prods = Array.init (Array.length indexed_prods) (fun i ->

    let prod = indexed_prods.(i) in
    let rhs_length = List.length prod.right in

    (* one dottedproduction for every dot position, which is one
     * more than the # of RHS elements *)
    Array.init (rhs_length + 1) (fun dot ->
      let dot_at_end   = dot = rhs_length in

      { prod; dot; dprod_id = next_id ();
        after_dot  = (if dot_at_end then None else Some (List.nth prod.right dot));
        first_set  = TerminalSet.create term_count;
        can_derive_empty = false;
        back_pointer = None;
      }
    )

  ) in

  (* the mapping is dense by construction, no need to verify it *)

  dotted_prods


let verify_nonshared indexed_nonterms indexed_prods dotted_prods =
  (* check nonterminals with nonterminals *)
  Array.iter (fun nonterm1 ->
    Array.iter (fun nonterm2 ->
      assert (nonterm1 == nonterm2 || nonterm1.first != nonterm2.first);
    ) indexed_nonterms
  ) indexed_nonterms;
  (* check productions with productions *)
  Array.iter (fun prod1 ->
    Array.iter (fun prod2 ->
      assert (prod1 == prod2 || prod1.first_rhs != prod2.first_rhs);
    ) indexed_prods
  ) indexed_prods;
  (* check dotted productions with dotted productions *)
  Array.iter (Array.iter (fun dprod1 ->
    Array.iter (Array.iter (fun dprod2 ->
      assert (dprod1 == dprod2 || dprod1.first_set != dprod2.first_set);
    )) dotted_prods
  )) dotted_prods;
  (* check nonterminals with productions *)
  Array.iter (fun nonterm ->
    Array.iter (fun prod ->
      assert (nonterm.first != prod.first_rhs);
    ) indexed_prods
  ) indexed_nonterms;
  (* check productions with dottedproductions *)
  Array.iter (fun prod ->
    Array.iter (Array.iter (fun dprod ->
      assert (prod.first_rhs != dprod.first_set);
    )) dotted_prods
  ) indexed_prods;
  (* check nonterminals with dotted productions *)
  Array.iter (fun nonterm ->
    Array.iter (Array.iter (fun dprod ->
      assert (nonterm.first != dprod.first_set);
    )) dotted_prods
  ) indexed_nonterms


let verify_empty indexed_nonterms indexed_prods dotted_prods =
  Array.iter (fun nonterm ->
    assert (TerminalSet.count nonterm.first = 0)
  ) indexed_nonterms;
  Array.iter (fun prod ->
    assert (TerminalSet.count prod.first_rhs = 0)
  ) indexed_prods;
  Array.iter (Array.iter (fun dprod ->
    assert (TerminalSet.count dprod.first_set = 0)
  )) dotted_prods


let init_env grammar =
  (* build indexed nonterminal map *)
  let indexed_nonterms = compute_indexed_nonterms grammar.nonterminals in
  let nonterm_count = Array.length indexed_nonterms in

  (* build indexed terminal map *)
  let indexed_terms = compute_indexed_terms grammar.terminals in
  let term_count = Array.length indexed_terms in

  (* build indexed production map *)
  let indexed_prods, prods_by_lhs = compute_indexed_prods grammar.productions nonterm_count in

  (* build dotted productions for each production *)
  let dotted_prods = compute_dotted_productions indexed_prods term_count in

  (* make the env *)
  let env = {
    indexed_nonterms;
    indexed_terms;
    indexed_prods;
    prods_by_lhs;
    dotted_prods;
    derivable = Derivability.initial_derivable_relation nonterm_count;
    cyclic_grammar = false;
    item_sets = [];
    start_state = None;
  } in

  (* reset first/follow sets to 0 *)
  reset_first_follow grammar.productions grammar.nonterminals term_count;

  if paranoid then (
    (* verify that no objects share a terminal set *)
    verify_nonshared indexed_nonterms indexed_prods dotted_prods;

    (* verify that all terminal set contain no terminals *)
    verify_empty indexed_nonterms indexed_prods dotted_prods;
  );

  env
