open AnalysisEnvType
open GrammarType


let output_token out term =
  Printf.fprintf out "%%token %-30s\n" term.tbase.name


let output_symbol out = function
  | Terminal (_, term) ->
      Printf.fprintf out " %s" term.tbase.name
  | Nonterminal (_, nonterm) ->
      Printf.fprintf out " %s" nonterm.nbase.name


let output_production out prod =
  output_string out "\t|";
  if prod.right = [] then
    output_string out " /* empty */"
  else
    List.iter (output_symbol out) prod.right;
  Printf.fprintf out "\t{ %d }\n" prod.prod_index


let output_nonterm out = function
  | [] -> ()
  | first :: _ as prods ->
      Printf.fprintf out "%s:\n" first.left.nbase.name;
      List.iter (output_production out) prods;
      output_string out "\n"


let output_grammar env =
  let out = open_out "grammar.mly" in

  output_string out "%{\n";
  output_string out "%}\n\n";
  Array.iter (output_token out) env.indexed_terms;
  let first = env.indexed_nonterms.(1) in
  Printf.fprintf out "\n%%start<int> %s\n\n" first.nbase.name;
  output_string out "%%\n\n";
  Array.iter (output_nonterm out) env.prods_by_lhs;

  close_out out
