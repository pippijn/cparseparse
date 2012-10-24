open GrammarType
open AnalysisEnvType


let print_terminal_set ?(out=stdout) ?(abbreviate=true) ?(name=".") terms set =
  let open GrammarType in
  (*Printf.fprintf out "[1;30mFirst(%s) = " name;*)
  Printf.fprintf out "First(%s) = " name;
  Printf.fprintf out "%d {" (TerminalSet.cardinal set);

  if abbreviate then (
    output_string out " ..."
  ) else (
    let first = ref true in
    TermArray.iter (fun term ->
      if TerminalSet.mem term.tbase.index_id set then (
        if not !first then
          output_string out ",";
        output_string out " ";
        first := false;

        let name =
          if term.alias <> "" then
            term.alias
          else
            term.tbase.name
        in
        output_string out name
      )
    ) terms
  );
  (*output_string out " }[0m"*)
  output_string out " }"


let print_dotted_production ?(out=stdout) index dprod =
  let prod = ProdArray.get index.prods dprod.prod in
  let left = NtArray.get index.nonterms prod.left in

  output_string out left.nbase.name;
  output_string out " ->";
  BatList.iteri (fun position rhs ->
    output_string out " ";
    let after_dot =
      match dprod.after_dot with
      | None -> false
      | Some sym -> rhs == sym
    in
    if position = dprod.dot then
      output_string out ".[";
    PrintGrammar.print_symbol ~out index.terms index.nonterms rhs;
    if after_dot then
      output_string out "]";
  ) prod.right;

  if dprod.dot = List.length prod.right then
    (* dot is at end *)
    output_string out " .";
  
  output_string out "  ";
  print_terminal_set ~out index.terms dprod.first_set


let print_lr_item ?(out=stdout) env item =
  print_dotted_production ~out env.index item.dprod;
  output_string out "  ";
  print_terminal_set ~out env.index.terms item.lookahead


let print_item_set ?(out=stdout) ?(print_nonkernels=false) env item_set =
  let print_lr_item item =
    (* print its text *)
    output_string out "  ";
    print_lr_item ~out env item;
    output_string out "\t\t";

    (* print any transitions on its after-dot symbol *)
    begin match LrItem.symbol_after_dot item with
    | None -> () (* dot is at end *)
    | Some sym ->
        match ItemSet.transition item_set sym with
        | None -> output_string out "(no transition)"
        | Some is -> Printf.fprintf out "--> %a" Ids.State.print is.state_id
    end;

    output_string out "\n";
  in

  Printf.fprintf out "State %a,\n  sample input: %s\n  and left context: %s\n"
    Ids.State.print item_set.state_id
    (SampleInput.sample_input env.index.terms env.index.nonterms env.index.prods env.prods_by_lhs item_set)
    (SampleInput.left_context env.index.terms env.index.nonterms item_set);

  Printf.fprintf out "ItemSet %a {\n" Ids.State.print item_set.state_id;
  if print_nonkernels then (
    List.iter print_lr_item
      (List.sort compare
        (item_set.kernel_items.items @ item_set.nonkernel_items));
  ) else (
    List.iter print_lr_item
      (List.sort compare
        item_set.kernel_items.items);
  );
  output_string out "\n";

  (* print transition function directly, since I'm now throwing
   * away items sometimes *)
  TermArray.iteri (fun i transition ->
    match transition with
    | None -> ()
    | Some transition ->
        Printf.fprintf out "  on terminal %s go to %a\n"
          (TermArray.get env.index.terms i).tbase.name
          Ids.State.print transition.state_id
  ) item_set.term_transition;

  NtArray.iteri (fun i transition ->
    match transition with
    | None -> ()
    | Some transition ->
        Printf.fprintf out "  on nonterminal %s go to %a\n"
          (NtArray.get env.index.nonterms i).nbase.name
          Ids.State.print transition.state_id
  ) item_set.nonterm_transition;

  List.iter (fun item ->
    output_string out "  can reduce by ";
    let prod = ProdArray.get env.index.prods item.dprod.prod in
    PrintGrammar.print_production ~out env.index.terms env.index.nonterms prod;
    output_string out "\n";
  ) item_set.dots_at_end;

  output_string out "}\n\n";
  flush out
