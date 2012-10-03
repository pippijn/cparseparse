open Gramtype
open AnalysisEnvType


(* clear first/follow sets by intersecting them with 0 *)
let reset_first_follow prods nonterms term_count =
  let empty = TerminalSet.empty () in
  let reset set = TerminalSet.intersect set empty in

  Stringmap.iter (fun _ nonterm ->
    reset nonterm.first;
    reset nonterm.follow;
  ) nonterms;

  List.iter (fun prod ->
    reset prod.first_set
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


let compute_dotted_productions indexed_prods =
  let dotted_prods = Array.init (Array.length indexed_prods) (fun i ->
    let prod = indexed_prods.(i) in
    let rhs_length = List.length prod.right in
    (* one dottedproduction for every dot position, which is one
     * more than the # of RHS elements *)
    Array.init (rhs_length + 1) (fun dot ->
      let dot_at_start = dot = 0 in
      let dot_at_end   = dot = rhs_length in

      { prod; dot;
        before_dot = (if dot_at_start then None else Some (List.nth prod.right (dot - 1)));
        after_dot  = (if dot_at_end   then None else Some (List.nth prod.right  dot));
      }
    )
  ) in

  dotted_prods


let init_env grammar =
  (* number of nonterminals + 1 for empty_nonterminal *)
  let nonterm_count = Stringmap.cardinal grammar.nonterminals + 1 in
  let term_count = Stringmap.cardinal grammar.terminals in

  reset_first_follow grammar.productions grammar.nonterminals term_count;

  let indexed_nonterms = compute_indexed_nonterms grammar.nonterminals
  and indexed_terms = compute_indexed_terms grammar.terminals
  and indexed_prods, prods_by_lhs = compute_indexed_prods grammar.productions nonterm_count
  in

  let dotted_prods = compute_dotted_productions indexed_prods in

  let env = {
    indexed_nonterms;
    indexed_terms;
    indexed_prods;
    prods_by_lhs;
    dotted_prods;
    derivable = Derivability.initial_derivable_relation nonterm_count;
    cyclic_grammar = false;
  } in

  env
