open AnalysisEnvType
open GrammarType

let (|>) = BatPervasives.(|>)


(************************************************************
 * :: General reachability of grammar elements
 ************************************************************)

let set_nt_reachable nreach nonterm =
  assert (not (NtSet.mem nreach nonterm.nt_index));
  NtSet.add nreach nonterm.nt_index

let set_term_reachable treach term =
  assert (not (TermSet.mem treach term.term_index));
  TermSet.add treach term.term_index


let rec compute_reachable_dfs nreach treach prods prods_by_lhs nonterm =
  (* if we did not see this nonterminal, yet *)
  if not (NtSet.mem nreach nonterm.nt_index) then (
    set_nt_reachable nreach nonterm;

    (* iterate over this nonterminal's rules *)
    List.iter (fun prod_index ->
      let prod = ProdArray.get prods prod_index in

      (* iterate over symbols in the rule RHS *)
      List.iter (function
        | Nonterminal (_, nonterm) ->
            (* recursively analyze nonterminal elements *)
            compute_reachable_dfs nreach treach prods prods_by_lhs nonterm
        | Terminal (_, term) ->
            (* just mark terminals *)
            TermSet.add treach term.term_index
      ) prod.right
    ) (NtArray.get prods_by_lhs nonterm.nt_index)
  )


let compute_reachable nonterms terms prods prods_by_lhs start =
  (* start by clearing the reachability flags *)
  let nreach = NtSet.create (NtArray.length nonterms) in
  let treach = TermSet.create (TermArray.length terms) in

  (* do a DFS on the grammar, marking things reachable as
   * they're encountered *)
  compute_reachable_dfs nreach treach prods prods_by_lhs start;

  (* the empty and start symbol are reachable *)
  let empty = NtArray.get nonterms StateId.Nonterminal.empty in
  assert (empty == empty_nonterminal);
  set_nt_reachable nreach empty;

  let start = NtArray.get nonterms StateId.Nonterminal.start in
  assert (start.nbase.name == GrammarTreeParser.start_name);
  set_nt_reachable nreach start;

  (* the EOF token is reachable *)
  let eof = TermArray.get terms StateId.Terminal.eof in
  set_term_reachable treach eof;

  nreach, treach



(************************************************************
 * :: Reachability via tagged symbols
 ************************************************************)

let rec compute_reachable_tagged_dfs prods prods_by_lhs reachable prod_index =
  let prod = ProdArray.get prods prod_index in

  Hashtbl.add reachable prod.left.nbase.name ();

  List.iter (function
    (* untagged nonterminals are ignored *)
    | Nonterminal ("", _)
    | Terminal _ -> ()

    | Nonterminal (_, { nt_index; nbase = { name } }) ->
        if not (Hashtbl.mem reachable name) then
          (* recurse into the nonterminal's productions *)
          List.iter
            (compute_reachable_tagged_dfs prods prods_by_lhs reachable)
            (NtArray.get prods_by_lhs nt_index)
  ) prod.right


let compute_reachable_tagged prods prods_by_lhs =
  (* start at the first production *)
  let first_production =
    StateId.Nonterminal.start
    |> NtArray.get prods_by_lhs
    |> List.hd
  in

  let reachable = Hashtbl.create 13 in
  compute_reachable_tagged_dfs prods prods_by_lhs reachable first_production;

  Hashtbl.fold (fun key () reachable ->
    StringSet.add key reachable
  ) reachable StringSet.empty
