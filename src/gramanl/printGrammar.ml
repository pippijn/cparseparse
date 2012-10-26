open GrammarType

let print_tag out = function
  | None -> ()
  | Some tag ->
      let tag = Sloc.value tag in
      Printf.fprintf out "%s:" tag


let print_symbol ?(out=stdout) terms nonterms sym =
  match sym with
  | Terminal (tag, term) ->
      print_tag out tag;
      output_string out (GrammarUtil.name_of_symbol terms nonterms sym)
  | Nonterminal (tag, nonterm) ->
      print_tag out tag;
      output_string out (GrammarUtil.name_of_symbol terms nonterms sym)


let print_production ?(out=stdout) terms nonterms prod =
  let left = NtArray.get nonterms prod.left in
  Printf.fprintf out "  [%a (%a)] %s ->"
    Ids.Production.print prod.pbase.index_id
    Ids.Production.print prod.pbase.index_id
    left.nbase.name;
  if prod.right = [] then
    output_string out " empty"
  else
    List.iter (fun sym -> output_string out " "; print_symbol ~out terms nonterms sym) prod.right;
  if prod.prec <> 0 then
    Printf.fprintf out " %%prec(%d)" prod.prec
