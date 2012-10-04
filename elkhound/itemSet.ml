open AnalysisEnvType

(************************************************************
 * :: Functions
 ************************************************************)


let transition item_set sym =
  let open GrammarType in
  match sym with
  | Terminal (_, term) ->
      item_set.term_transition.(term.term_index)
  | Nonterminal (_, nonterm) ->
      item_set.nonterm_transition.(nonterm.nt_index)


let set_transition from_set sym to_set =
  let open GrammarType in
  match sym with
  | Terminal (_, term) ->
      from_set.term_transition.(term.term_index) <- Some to_set
  | Nonterminal (_, nonterm) ->
      from_set.nonterm_transition.(nonterm.nt_index) <- Some to_set
