open Sexplib.Conv
open GrammarType

let (|>) = BatPervasives.(|>)

type t = {
  index 	: GrammarType.index;
  prods_by_lhs  : (Ids.Production.t list, Sig.readonly) NtArray.t;
  reachable	: Sig.readonly NtSet.t;
  verbatims	: GrammarType.global_semantic SemanticVariant.variants;
}


let compute_indexed_nonterms nonterms =
  let indexed = NtArray.make (LocStringMap.cardinal nonterms + 1) empty_nonterminal in

  (* indexed.(0) is empty_nonterminal *)
  assert (Ids.Nonterminal.is_empty empty_nonterminal.nbase.index_id);

  LocStringMap.iter (fun _ nonterm ->
    (* the ids have already been assigned *)
    let i = nonterm.nbase.index_id in (* map: symbol to index *)
    (* verify there are no duplicate indices *)
    let existing = NtArray.get indexed i in
    if existing != empty_nonterminal then (
      Printf.printf "%a has the same index (%a) as %a\n"
        Sloc.print_string existing.nbase.name
        Ids.Nonterminal.print i
        Sloc.print_string nonterm.nbase.name
    );
    assert (existing == empty_nonterminal);
    NtArray.set indexed i nonterm (* map: index to symbol *)
  ) nonterms;

  (* verify invariants *)
  NtArray.iteri (fun nt_index nonterm ->
    (* the mapping must be correct *)
    assert (nonterm.nbase.index_id == nt_index);

    (* "empty" must be the first nonterminal *)
    if Ids.Nonterminal.is_empty nonterm.nbase.index_id then
      assert (nonterm == empty_nonterminal)

    (* the synthesised start symbol must follow *)
    else if Ids.Nonterminal.is_start nonterm.nbase.index_id then
      assert (nonterm.nbase.name == GrammarTreeParser.start_name)

    (* any other nonterminals must not be empty *)
    else
      assert (nonterm != empty_nonterminal)
  ) indexed;

  (* number of nonterminals + 1 for empty_nonterminal *)
  assert (NtArray.length indexed == LocStringMap.cardinal nonterms + 1);

  NtArray.readonly indexed


let compute_indexed_terms terms =
  let indexed = TermArray.make (LocStringMap.cardinal terms) empty_terminal in

  LocStringMap.iter (fun _ term ->
    (* the ids have already been assigned *)
    let i = term.tbase.index_id in (* map: symbol to index *)
    TermArray.set indexed i term (* map: index to symbol *)
  ) terms;

  (* verify we filled the term_index map *)
  TermArray.iter (fun term -> assert (term != empty_terminal)) indexed;

  assert (TermArray.length indexed == LocStringMap.cardinal terms);

  TermArray.readonly indexed


let compute_indexed_prods productions nonterm_count =
  (* map: prod_index -> production *)
  let indexed = ProdArray.make (List.length productions) empty_production in
  (* map: nonterminal -> productions with that nonterm on LHS *)
  let prods_by_lhs = NtArray.make nonterm_count [] in

  (* fill in both maps *)
  BatList.iteri (fun i prod ->
    let prod_index = Ids.Production.of_int i in
    let nt_index = prod.left in

    assert (prod.pbase.index_id == prod_index);

    begin
      let prods = NtArray.get prods_by_lhs nt_index in
      NtArray.set prods_by_lhs nt_index (prod_index :: prods);
    end;

    assert (ProdArray.get indexed prod_index == empty_production);
    ProdArray.set indexed prod_index prod;
  ) productions;

  (* verify invariants *)
  NtArray.iteri (fun nt_index ->
    List.iter (fun prod_index ->
      let prod = ProdArray.get indexed prod_index in
      assert (prod.left == nt_index);
    )
  ) prods_by_lhs;
  ProdArray.iteri (fun prod_index prod ->
    assert (prod.pbase.index_id == prod_index);
  ) indexed;

  (* verify we filled the prod_index map *)
  ProdArray.iter (fun prod -> assert (prod != empty_production)) indexed;

  ProdArray.readonly indexed,
  NtArray.readonly prods_by_lhs


let compute_indices grammar =
  (* build indexed terminal map *)
  let terms = compute_indexed_terms grammar.terminals in

  (* build indexed nonterminal map *)
  let nonterms = compute_indexed_nonterms grammar.nonterminals in

  (* build indexed production map *)
  let prods, prods_by_lhs = compute_indexed_prods grammar.productions (NtArray.length nonterms) in

  let reachable = Reachability.compute_reachable_tagged prods prods_by_lhs in

  (* construct parse tree variants *)
  let nonterms =
    nonterms
    |> PtreeMaker.nonterms SemanticVariant.Ptree reachable
    |> PtreeMaker.nonterms SemanticVariant.Treematch reachable
  in

  let prods =
    prods
    |> PtreeMaker.prods SemanticVariant.Ptree reachable nonterms prods_by_lhs
    |> PtreeMaker.prods SemanticVariant.Treematch reachable nonterms prods_by_lhs
  in

  let verbatims =
    grammar.verbatim
    |> SemanticVariant.combine (PtreeMaker.verbatims SemanticVariant.Ptree)
    |> SemanticVariant.combine (PtreeMaker.verbatims SemanticVariant.Treematch)
  in

  let index = { terms; nonterms; prods } in
  
  { index; prods_by_lhs; reachable; verbatims }
