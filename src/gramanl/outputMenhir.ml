open AnalysisEnvType
open GrammarType


let output_token out term =
  if term.tbase.reachable then
    Printf.fprintf out "%%token %-30s\n" term.tbase.name


let output_assoc = let open Assoc in function
  | AK_LEFT -> "%left"
  | AK_RIGHT -> "%right"
  | AK_NONASSOC
  | AK_NEVERASSOC
  | AK_SPLIT -> "%nonassoc"


let output_precs out terms =
  let max_prec = Array.fold_left (fun prec term -> max prec term.precedence) 0 terms in
  let precs = Array.make (max_prec + 1) [] in
  Array.iter (fun term ->
    if term.precedence <> 0 then
      precs.(term.precedence) <- term :: precs.(term.precedence)
  ) terms;

  Array.iter (function
    | [] -> ()
    | first :: _ as terms ->
        output_string out (output_assoc first.associativity);
        List.iter (fun term -> output_string out (" " ^ term.tbase.name)) terms;
        output_string out "\n"

  ) precs;

  ()


let output_symbol out = function
  | Terminal (_, term) ->
      Printf.fprintf out " %s" term.tbase.name
  | Nonterminal (_, nonterm) ->
      Printf.fprintf out " %s" nonterm.nbase.name


let last_prec =
  List.fold_left (fun prec -> function
    | Terminal (_, term) -> term.precedence
    | _ -> prec
  ) 0


let output_production out terms prod =
  output_string out "\t|";
  if prod.right = [] then
    output_string out " /* empty */"
  else
    List.iter (output_symbol out) prod.right;
  if prod.prec <> 0 && prod.prec <> last_prec prod.right then (
    let term = BatArray.find (fun term -> term.precedence = prod.prec) terms in
    Printf.fprintf out " %%prec %s" term.tbase.name;
  );
  Printf.fprintf out "\t{ %d }\n" prod.prod_index


let output_nonterm out terms = function
  | [] -> ()
  | first :: _ as prods ->
      if first.left.nbase.reachable then (
        Printf.fprintf out "%s:\n" first.left.nbase.name;
        List.iter (output_production out terms) prods;
        output_string out "\n";
      )


let output_grammar ~file env =
  let out = open_out file in

  output_string out "%{\n";
  output_string out "%}\n\n";
  Array.iter (output_token out) env.indexed_terms;
  output_string out "\n";
  output_precs out env.indexed_terms;
  let first = env.indexed_nonterms.(1) in
  Printf.fprintf out "\n%%start<int> %s\n\n" first.nbase.name;
  output_string out "%%\n\n";
  Array.iter (output_nonterm out env.indexed_terms) env.prods_by_lhs;

  close_out out
