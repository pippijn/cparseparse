open GrammarType

let print_symbol sym =
  match sym with
  | Terminal ("", term) ->
      print_string (Grammar.name_of_symbol sym)
  | Terminal (tag, term) ->
      Printf.printf "%s:" tag;
      print_string (Grammar.name_of_symbol sym)
  | Nonterminal ("", nonterm) ->
      print_string (Grammar.name_of_symbol sym)
  | Nonterminal (tag, nonterm) ->
      Printf.printf "%s:" tag;
      print_string (Grammar.name_of_symbol sym)

let print_production prod =
  Printf.printf "[%d] %s ->" prod.prod_index prod.left.nbase.name;
  List.iter (fun sym -> print_string " "; print_symbol sym) prod.right
