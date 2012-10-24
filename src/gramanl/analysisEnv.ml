open AnalysisEnvType
open GrammarType


(* clear first/follow sets *)
let reset_first_follow prods nonterms =
  StringMap.iter (fun _ nonterm ->
    nonterm.first <- TerminalSet.empty;
    nonterm.follow <- TerminalSet.empty;
  ) nonterms;

  List.iter (fun prod ->
    prod.first_rhs <- TerminalSet.empty;
  ) prods


let compute_indexed_nonterms nonterms =
  let indexed = NtArray.make (StringMap.cardinal nonterms + 1) empty_nonterminal in

  (* indexed.(0) is empty_nonterminal *)
  assert (StateId.Nonterminal.is_empty empty_nonterminal.nt_index);

  StringMap.iter (fun _ nonterm ->
    (* the ids have already been assigned *)
    let i = nonterm.nt_index in (* map: symbol to index *)
    (* verify there are no duplicate indices *)
    let existing = NtArray.get indexed i in
    if existing != empty_nonterminal then (
      Printf.printf "%s has the same index (%a) as %s\n"
        existing.nbase.name
        StateId.Nonterminal.print i
        nonterm.nbase.name
    );
    assert (existing == empty_nonterminal);
    NtArray.set indexed i nonterm (* map: index to symbol *)
  ) nonterms;

  (* verify invariants *)
  NtArray.iteri (fun nt_index nonterm ->
    (* the mapping must be correct *)
    assert (nonterm.nt_index == nt_index);

    (* "empty" must be the first nonterminal *)
    if StateId.Nonterminal.is_empty nonterm.nt_index then
      assert (nonterm == empty_nonterminal)

    (* the synthesised start symbol must follow *)
    else if StateId.Nonterminal.is_start nonterm.nt_index then
      assert (nonterm.nbase.name == GrammarTreeParser.start_name)

    (* any other nonterminals must not be empty *)
    else
      assert (nonterm != empty_nonterminal)
  ) indexed;

  (* number of nonterminals + 1 for empty_nonterminal *)
  assert (NtArray.length indexed = StringMap.cardinal nonterms + 1);

  NtArray.readonly indexed


let compute_indexed_terms terms =
  let indexed = TermArray.make (StringMap.cardinal terms) empty_terminal in

  StringMap.iter (fun _ term ->
    (* the ids have already been assigned *)
    let i = term.term_index in (* map: symbol to index *)
    TermArray.set indexed i term (* map: index to symbol *)
  ) terms;

  (* verify we filled the term_index map *)
  TermArray.iter (fun term -> assert (term != empty_terminal)) indexed;

  assert (TermArray.length indexed = StringMap.cardinal terms);

  TermArray.readonly indexed


let compute_indexed_prods productions nonterm_count =
  (* map: prod_index -> production *)
  let indexed = ProdArray.make (List.length productions) empty_production in
  (* map: nonterminal -> productions with that nonterm on LHS *)
  let prods_by_lhs = NtArray.make nonterm_count [] in

  (* fill in both maps *)
  BatList.iteri (fun i production ->
    let prod_index = StateId.Production.of_int i in
    let nt_index = production.left in

    begin
      let prods = NtArray.get prods_by_lhs nt_index in
      NtArray.set prods_by_lhs nt_index (prod_index :: prods);
    end;

    assert (ProdArray.get indexed prod_index == empty_production);
    production.prod_index <- prod_index;
    ProdArray.set indexed prod_index production;
  ) productions;

  (* verify invariants *)
  NtArray.iteri (fun nt_index ->
    List.iter (fun prod_index ->
      let prod = ProdArray.get indexed prod_index in
      assert (prod.left == nt_index);
    )
  ) prods_by_lhs;
  ProdArray.iteri (fun prod_index prod ->
    assert (prod.prod_index == prod_index);
  ) indexed;

  (* verify we filled the prod_index map *)
  ProdArray.iter (fun prod -> assert (prod != empty_production)) indexed;

  ProdArray.readonly indexed,
  NtArray.readonly prods_by_lhs


let compute_dotted_productions indexed_prods =
  let open AnalysisEnvType in

  let next_id =
    let next = ref 0 in
    fun () ->
      let id = !next in
      incr next;
      id
  in

  let dotted_prods = ProdArray.init (ProdArray.length indexed_prods) (fun i ->

    let prod = ProdArray.get indexed_prods i in
    let rhs_length = List.length prod.right in

    (* one dottedproduction for every dot position, which is one
     * more than the # of RHS elements *)
    Array.init (rhs_length + 1) (fun dot ->
      let dot_at_end = dot = rhs_length in

      {
        prod = prod.prod_index;
        dot;
        dprod_id = next_id ();
        after_dot  = (if dot_at_end then None else Some (List.nth prod.right dot));
        can_derive_empty = false;
        first_set = TerminalSet.empty;
        back_pointer = None;
      }
    )

  ) in

  (* the mapping is dense by construction, no need to verify it *)

  (* it is already readonly, too *)
  dotted_prods


let verify_nonshared indexed_nonterms indexed_prods dotted_prods =
  let open AnalysisEnvType in

  (* check nonterminals with nonterminals *)
  NtArray.iter (fun nonterm1 ->
    NtArray.iter (fun nonterm2 ->
      assert (nonterm1 == nonterm2 || nonterm1.first != nonterm2.first);
    ) indexed_nonterms
  ) indexed_nonterms;
  (* check productions with productions *)
  ProdArray.iter (fun prod1 ->
    ProdArray.iter (fun prod2 ->
      assert (prod1 == prod2 || prod1.first_rhs != prod2.first_rhs);
    ) indexed_prods
  ) indexed_prods;
  (* check dotted productions with dotted productions *)
  ProdArray.iter (Array.iter (fun dprod1 ->
    ProdArray.iter (Array.iter (fun dprod2 ->
      assert (dprod1 == dprod2 || dprod1.first_set != dprod2.first_set);
    )) dotted_prods
  )) dotted_prods;
  (* check nonterminals with productions *)
  NtArray.iter (fun nonterm ->
    ProdArray.iter (fun prod ->
      assert (nonterm.first != prod.first_rhs);
    ) indexed_prods
  ) indexed_nonterms;
  (* check productions with dottedproductions *)
  ProdArray.iter (fun prod ->
    ProdArray.iter (Array.iter (fun dprod ->
      assert (prod.first_rhs != dprod.first_set);
    )) dotted_prods
  ) indexed_prods;
  (* check nonterminals with dotted productions *)
  NtArray.iter (fun nonterm ->
    ProdArray.iter (Array.iter (fun dprod ->
      assert (nonterm.first != dprod.first_set);
    )) dotted_prods
  ) indexed_nonterms


let verify_empty indexed_nonterms indexed_prods dotted_prods =
  NtArray.iter (fun nonterm ->
    assert (TerminalSet.cardinal nonterm.first = 0)
  ) indexed_nonterms;
  ProdArray.iter (fun prod ->
    assert (TerminalSet.cardinal prod.first_rhs = 0)
  ) indexed_prods;
  ProdArray.iter (Array.iter (fun dprod ->
    assert (TerminalSet.cardinal dprod.first_set = 0)
  )) dotted_prods


let init_env grammar =
  let start_nt =
    (StringMap.find grammar.start_symbol grammar.nonterminals).nt_index
  in

  (* build indexed terminal map *)
  let indexed_terms = compute_indexed_terms grammar.terminals in

  (* build indexed nonterminal map *)
  let indexed_nonterms = compute_indexed_nonterms grammar.nonterminals in
  let nonterm_count = NtArray.length indexed_nonterms in

  (* build indexed production map *)
  let indexed_prods, prods_by_lhs = compute_indexed_prods grammar.productions nonterm_count in

  let reachable = Reachability.compute_reachable_tagged indexed_prods prods_by_lhs in

  let transform prefix =
    let module Transform =
      PtreeMaker.Make(struct
        let prefix = prefix
      end)
    in

    {
      prefix;
      variant_nonterms = Transform.nonterms reachable indexed_nonterms;
      variant_prods    = Transform.prods    reachable indexed_nonterms prods_by_lhs indexed_prods;
      (* drop verbatim sections *)
      verbatims = [];
      impl_verbatims = [];
    }
  in
  (* construct parse tree variants *)
  let variants = [
    (* user actions *)
    {
      prefix = "";
      variant_nonterms = indexed_nonterms;
      variant_prods = indexed_prods;
      verbatims = grammar.verbatim;
      impl_verbatims = grammar.impl_verbatim;
    };
    transform "Ptree";
    transform "Treematch";
  ] in

  (* build dotted productions for each production *)
  let dotted_prods = compute_dotted_productions indexed_prods in

  (* make the env *)
  let env = {
    index = {
      nonterms = indexed_nonterms;
      terms = indexed_terms;
      prods = indexed_prods;
    };
    start_nt;
    reachable;
    prods_by_lhs;
    dotted_prods;
    derivable = Derivability.initial_derivable_relation nonterm_count;
    cyclic_grammar = false;
    start_state = None;

    options = grammar.config;
    variants;
  } in

  (* reset first/follow sets to 0 *)
  reset_first_follow grammar.productions grammar.nonterminals;

  if Options._paranoid () then (
    (* verify that no objects share a terminal set *)
    verify_nonshared indexed_nonterms indexed_prods dotted_prods;

    (* verify that all terminal set contain no terminals *)
    verify_empty indexed_nonterms indexed_prods dotted_prods;
  );

  env
