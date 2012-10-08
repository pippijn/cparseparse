open AnalysisEnvType

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
  ) false [item_set.kernel_items; item_set.nonkernel_items]


let possible_reductions item_set lookahead =
  let open GrammarType in
  List.fold_left (fun reductions item ->
    if Config.use_LR0 then (
      (* don't check the lookahead *)
      reductions

    ) else if Config.use_SLR1 then (
      (* the follow of its LHS must include 'lookahead' *)
      let prod = item.dprod.prod in
      if TerminalSet.is_set prod.left.follow lookahead.term_index then
        prod :: reductions
      else (
        if Config.trace_reductions then (
          Printf.printf "state %d, not reducing by "
            (int_of_state_id item_set.state_id);
          PrintGrammar.print_production prod;
          Printf.printf " because %s is not in follow of %s\n"
            lookahead.tbase.name
            prod.left.nbase.name;
        );
        reductions
      )

    ) else if Config.use_LALR1 || Config.use_LR1 then (
      (* the item's lookahead must include 'lookahead' *)
      let prod = item.dprod.prod in
      if TerminalSet.is_set item.lookahead lookahead.term_index then (
        if Config.trace_reductions then (
          Printf.printf "state %d, reducing by "
            (int_of_state_id item_set.state_id);
          PrintGrammar.print_production prod;
          Printf.printf " because %s is in lookahead\n"
            lookahead.tbase.name;
        );
        prod :: reductions
      ) else (
        if Config.trace_reductions then (
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