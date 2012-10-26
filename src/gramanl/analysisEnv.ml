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
    IntegralIndexedArray.init (rhs_length + 1) (fun dot ->
      let dot_at_end = dot = rhs_length in

      {
        prod = prod.pbase.index_id;
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


let init_env grammar =
  let start_nt =
    (StringMap.find (Sloc.value grammar.start_symbol) grammar.nonterminals).nbase.index_id
  in

  let indices = GrammarIndex.compute_indices grammar in

  let index        = indices.GrammarIndex.index in
  let reachable    = indices.GrammarIndex.reachable in
  let prods_by_lhs = indices.GrammarIndex.prods_by_lhs in
  let verbatims    = indices.GrammarIndex.verbatims in

  (* build dotted productions for each production *)
  let dotted_prods = compute_dotted_productions index.prods in

  (* make the env *)
  let env = {
    index;
    start_nt;
    reachable;
    prods_by_lhs;
    dotted_prods;
    derivable = Derivability.initial_derivable_relation (NtArray.length index.nonterms);
    cyclic_grammar = false;
    start_state = None;

    options = grammar.config;
    verbatims;
  } in

  (* reset first/follow sets to 0 *)
  reset_first_follow grammar.productions grammar.nonterminals;

  env
