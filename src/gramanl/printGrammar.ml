open GrammarType

let print_symbol ?(out=stdout) sym =
  match sym with
  | Terminal ("", term) ->
      output_string out (GrammarUtil.name_of_symbol sym)
  | Terminal (tag, term) ->
      Printf.fprintf out "%s:" tag;
      output_string out (GrammarUtil.name_of_symbol sym)
  | Nonterminal ("", nonterm) ->
      output_string out (GrammarUtil.name_of_symbol sym)
  | Nonterminal (tag, nonterm) ->
      Printf.fprintf out "%s:" tag;
      output_string out (GrammarUtil.name_of_symbol sym)


let print_production ?(out=stdout) nonterms prod =
  let left = NtArray.get nonterms prod.left in
  Printf.fprintf out "  [%a] %s ->" StateId.Production.print prod.prod_index left.nbase.name;
  if prod.right = [] then
    output_string out " empty"
  else
    List.iter (fun sym -> output_string out " "; print_symbol ~out sym) prod.right;
  if prod.prec <> 0 then
    Printf.fprintf out " %%prec(%d)" prod.prec
