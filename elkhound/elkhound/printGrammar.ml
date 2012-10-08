open GrammarType

let print_symbol ?(out=stdout) sym =
  match sym with
  | Terminal ("", term) ->
      output_string out (Grammar.name_of_symbol sym)
  | Terminal (tag, term) ->
      Printf.fprintf out "%s:" tag;
      output_string out (Grammar.name_of_symbol sym)
  | Nonterminal ("", nonterm) ->
      output_string out (Grammar.name_of_symbol sym)
  | Nonterminal (tag, nonterm) ->
      Printf.fprintf out "%s:" tag;
      output_string out (Grammar.name_of_symbol sym)


let print_production ?(out=stdout) prod =
  Printf.fprintf out "  [%d] %s ->" prod.prod_index prod.left.nbase.name;
  if prod.right = [] then
    output_string out " empty"
  else
    List.iter (fun sym -> output_string out " "; print_symbol ~out sym) prod.right;
  if prod.prec <> 0 then
    Printf.fprintf out " %%prec(%d)" prod.prec
