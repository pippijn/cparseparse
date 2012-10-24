open AnalysisEnvType
open GrammarType


let output_token out term =
  if (*term.tbase.reachable*) true then
    Printf.fprintf out "%%token %-30s\n" term.tbase.name


let output_assoc = let open Assoc in function
  | AK_LEFT -> "%left"
  | AK_RIGHT -> "%right"
  | AK_NONASSOC
  | AK_NEVERASSOC
  | AK_SPLIT -> "%nonassoc"


let output_precs out terms =
  let max_prec =
    TermArray.fold_left (fun prec term ->
      max prec term.precedence
    ) 0 terms
  in

  let precs = Array.make (max_prec + 1) [] in

  TermArray.iter (fun term ->
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


let output_symbol nonterms out = function
  | Terminal (_, term) ->
      Printf.fprintf out " %s" term.tbase.name
  | Nonterminal (_, nonterm) ->
      let nonterm = NtArray.get nonterms nonterm in
      Printf.fprintf out " %s" nonterm.nbase.name


let last_prec =
  List.fold_left (fun prec -> function
    | Terminal (_, term) -> term.precedence
    | _ -> prec
  ) 0


let output_production out index prod_index =
  let prod = ProdArray.get index.prods prod_index in

  output_string out "\t|";
  if prod.right = [] then
    output_string out " /* empty */"
  else
    List.iter (output_symbol index.nonterms out) prod.right;
  if prod.prec <> 0 && prod.prec <> last_prec prod.right then (
    let term = TermArray.find (fun term -> term.precedence = prod.prec) index.terms in
    Printf.fprintf out " %%prec %s" term.tbase.name;
  );
  Printf.fprintf out "\t{ %a }\n" StateId.Production.print prod.prod_index


let output_nonterm out index = function
  | [] -> ()
  | first_index :: _ as indices ->
      let first = ProdArray.get index.prods first_index in
      if (*first.left.nbase.reachable*) true then (
        let left = NtArray.get index.nonterms first.left in
        Printf.fprintf out "%s:\n" left.nbase.name;
        List.iter (output_production out index) indices;
        output_string out "\n";
      )


let output_grammar ~file env =
  let out = open_out file in

  output_string out "%{\n";
  output_string out "%}\n\n";
  TermArray.iter (output_token out) env.index.terms;
  output_string out "\n";
  output_precs out env.index.terms;
  let first = NtArray.get env.index.nonterms StateId.Nonterminal.start in
  Printf.fprintf out "\n%%start<int> %s\n\n" first.nbase.name;
  output_string out "%%\n\n";
  NtArray.iter (output_nonterm out env.index) env.prods_by_lhs;

  close_out out
