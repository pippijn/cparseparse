open GrammarType

let print_terminal { tbase = { name } } =
  print_string name

let print_nonterminal { nbase = { name } } =
  print_string name

let print_symbol = function
  | Terminal ("", term) ->
      print_terminal term
  | Terminal (tag, term) ->
      Printf.printf "%s:" tag;
      print_terminal term
  | Nonterminal ("", nonterm) ->
      print_nonterminal nonterm
  | Nonterminal (tag, nonterm) ->
      Printf.printf "%s:" tag;
      print_nonterminal nonterm

let print_production prod =
  Printf.printf "%s ->" prod.left.nbase.name;
  List.iter print_symbol prod.right;
  print_newline ()


let print_productions = List.iter print_production
