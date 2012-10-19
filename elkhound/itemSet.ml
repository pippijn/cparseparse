open AnalysisEnvType

(************************************************************
 * :: Common Operations
 ************************************************************)


module M = struct

  type t = item_set

  let hash a =
    ItemList.M.hash a.kernel_items

  let compare a b =
    (* since nonkernel items are entirely determined by kernel
     * items, and kernel items are sorted, it's sufficient to
     * check for kernel list equality *)
    ItemList.M.compare a.kernel_items b.kernel_items

  let equal a b =
    compare a b = 0

  let stats _ = failwith "Not supported"
  let reset _ = failwith "Not supported"

  let sexp_of_t = sexp_of_item_set
  let t_of_sexp = item_set_of_sexp

end

module Table = Hashtbl.Make(M)
module Map = SexpMap.Make(M)
module Set = SexpSet.Make(M)
module Stack = HashStack.Make(Table)


(************************************************************
 * :: Functions
 ************************************************************)


let transition_for_term item_set term =
  let open GrammarType in
  item_set.term_transition.(term.term_index)

let transition_for_nonterm item_set nonterm =
  let open GrammarType in
  item_set.nonterm_transition.(nonterm.nt_index)

let transition item_set sym =
  let open GrammarType in
  match sym with
  | Terminal (_, term) -> transition_for_term item_set term
  | Nonterminal (_, nonterm) -> transition_for_nonterm item_set nonterm


let set_transition_for_term from_set term to_set =
  let open GrammarType in
  from_set.term_transition.(term.term_index) <- Some to_set

let set_transition_for_nonterm from_set nonterm to_set =
  let open GrammarType in
  from_set.nonterm_transition.(nonterm.nt_index) <- Some to_set

let set_transition from_set sym to_set =
  let open GrammarType in
  match sym with
  | Terminal (_, term) -> set_transition_for_term from_set term to_set
  | Nonterminal (_, nonterm) -> set_transition_for_nonterm from_set nonterm to_set


let has_extending_shift item_set nonterm term =
  ListUtil.fold_left_many (fun result item ->
    result || LrItem.is_extending_shift item nonterm term
  ) false [item_set.kernel_items.items; item_set.nonkernel_items]


let possible_reductions item_set lookahead =
  let open GrammarType in
  List.fold_left (fun reductions item ->
    if Options._use_LR0 then (
      (* don't check the lookahead *)
      reductions

    ) else if Options._use_SLR1 then (
      (* the follow of its LHS must include 'lookahead' *)
      let prod = item.dprod.prod in
      if TerminalSet.mem lookahead.term_index prod.left.follow then
        prod :: reductions
      else (
        if Options._trace_reductions then (
          Printf.printf "state %d, not reducing by "
            (int_of_state_id item_set.state_id);
          PrintGrammar.print_production prod;
          Printf.printf " because %s is not in follow of %s\n"
            lookahead.tbase.name
            prod.left.nbase.name;
        );
        reductions
      )

    ) else if Options._use_LALR1 || Options._use_LR1 then (
      (* the item's lookahead must include 'lookahead' *)
      let prod = item.dprod.prod in
      if TerminalSet.mem lookahead.term_index item.lookahead then (
        if Options._trace_reductions then (
          Printf.printf "state %d, reducing by "
            (int_of_state_id item_set.state_id);
          PrintGrammar.print_production prod;
          Printf.printf " because %s is in lookahead\n"
            lookahead.tbase.name;
        );
        prod :: reductions
      ) else (
        if Options._trace_reductions then (
          Printf.printf "state %d, not reducing by "
            (int_of_state_id item_set.state_id);
          PrintGrammar.print_production prod;
          Printf.printf " because %s is not in lookahead\n"
            lookahead.tbase.name;
        );
        reductions
      )

    ) else (
      failwith "no LR variant specified"
    )
  ) [] item_set.dots_at_end
