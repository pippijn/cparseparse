open Gramtype

type env = {
  nonterms : nonterminal array;
  terms : terminal array;
  prods : production array;
  prods_by_lhs : production list array;
}


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
  let indexed = Array.make (Stringmap.cardinal nonterms) empty_nonterminal in

  let i =
    Stringmap.fold (fun _ nonterm i ->
      nonterm.nt_index <- i;
      indexed.(i) <- nonterm;
      i + 1
    ) nonterms 0
  in

  assert (i = Array.length indexed);
  Array.iter (fun nonterm -> assert (nonterm != empty_nonterminal)) indexed;

  indexed


let compute_indexed_terms terms =
  let indexed = Array.make (Stringmap.cardinal terms) empty_terminal in

  Stringmap.iter (fun _ term ->
    indexed.(term.term_index) <- term
  ) terms;

  Array.iter (fun term -> assert (term != empty_terminal)) indexed;

  indexed


let compute_indexed_prods productions nonterm_count =
  let indexed = Array.make (List.length productions) empty_production in
  let prods_by_lhs = Array.make nonterm_count [] in

  let i =
    List.fold_left (fun i production ->
      let nt_index = production.left.nt_index in
      prods_by_lhs.(nt_index) <- production :: prods_by_lhs.(nt_index);
      production.prod_index <- i;
      indexed.(i) <- production;
      i + 1
    ) 0 productions
  in

  assert (i = Array.length indexed);
  Array.iter (fun prod -> assert (prod != empty_production)) indexed;

  indexed, prods_by_lhs


let compute_dotted_productions productions =
  ()


let rec compute_reachable_dfs env nonterm =
  if nonterm.nbase.reachable then (
    ()
  ) else (
    nonterm.nbase.reachable <- true;

    List.iter (fun prod ->
      List.iter (function
        | Nonterminal (_, nonterm) ->
            compute_reachable_dfs env nonterm
        | Terminal (_, term) ->
            term.tbase.reachable <- true
      ) prod.right
    ) env.prods_by_lhs.(nonterm.nt_index)
  )


let compute_reachable env start =
  Array.iter (fun nonterm -> nonterm.nbase.reachable <- false) env.nonterms;
  Array.iter (fun term -> term.tbase.reachable <- false) env.terms;

  compute_reachable_dfs env start


let run_analyses grammar =
  let nonterm_count = Stringmap.cardinal grammar.nonterminals in
  let term_count = Stringmap.cardinal grammar.terminals in

  reset_first_follow grammar.productions grammar.nonterminals term_count;

  let nonterms = compute_indexed_nonterms grammar.nonterminals
  and terms = compute_indexed_terms grammar.terminals
  and prods, prods_by_lhs = compute_indexed_prods grammar.productions nonterm_count
  and dotted_prods = compute_dotted_productions grammar.productions
  in

  let env = { nonterms; terms; prods; prods_by_lhs; } in

  compute_reachable env grammar.start_symbol;

  ()
