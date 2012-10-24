open GrammarType


let name_of_terminal { tbase = { name }; alias } =
  if alias <> "" then
    alias
  else
    name


let name_of_nonterminal nonterms nt_index =
  let { nbase = { name } } = NtArray.get nonterms nt_index in
  name


let name_of_symbol nonterms = function
  | Nonterminal (_, nonterm) -> name_of_nonterminal nonterms nonterm
  | Terminal (_, term) -> name_of_terminal term


let tag_of_symbol = function
  | Nonterminal (tag, _)
  | Terminal (tag, _) -> tag


(* symbol equality ignores tags *)
let equal_symbol a b =
  match a, b with
  | Terminal (_, term_a), Terminal (_, term_b) ->
      term_a == term_b
  | Nonterminal (_, nonterm_a), Nonterminal (_, nonterm_b) ->
      nonterm_a == nonterm_b
  | _ ->
      (* terminals never equal non-terminals *)
      false


let compare_symbol a b =
  match a, b with
  (* any state with no incoming arcs (start state) is first *)
  | None, Some _ -> -1
  | Some _, None -> 1
  | None, None -> 0

  (* terminals come before nonterminals *)
  | Some (Nonterminal _), Some (Terminal _) -> 1
  | Some (Terminal _), Some (Nonterminal _) -> -1

  (* order by id within terms/nonterms *)
  | Some (Terminal (_, term_a)), Some (Terminal (_, term_b)) ->
      StateId.Terminal.compare term_a.term_index term_b.term_index
  | Some (Nonterminal (_, nonterm_a)), Some (Nonterminal (_, nonterm_b)) ->
      StateId.Nonterminal.compare nonterm_a nonterm_b


let rhs_has_nonterm prod nonterm =
  let open GrammarType in

  try
    ignore (List.find (function
      | Terminal _ -> false
      | Nonterminal (_, nt) -> nt == nonterm
    ) prod.right);
    true
  with Not_found ->
    false


let num_rhs_nonterms prod =
  let open GrammarType in

  ExtList.count (function
    | Terminal    _ -> false
    | Nonterminal _ -> true
  ) prod.right
