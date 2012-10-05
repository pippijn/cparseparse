open Batteries_uni
open GrammarType
open AnalysisEnvType


let print_dotted_production dprod =
  print_string dprod.prod.left.nbase.name;
  print_string " ->";
  List.iteri (fun position rhs ->
    print_string " ";
    let after_dot =
      match dprod.after_dot with
      | None -> false
      | Some sym -> rhs == sym
    in
    if position = dprod.dot then
      print_string ".[";
    PrintGrammar.print_symbol rhs;
    if after_dot then
      print_string "]";
  ) dprod.prod.right;

  if dprod.dot = List.length dprod.prod.right then
    (* dot is at end *)
    print_string " ."


let print_terminal_set ?(with_lookahead=true) terms set =
  let open GrammarType in
  Printf.printf "%d {" (TerminalSet.count set);

  if with_lookahead then (
    let first = ref true in
    Array.iter (fun term ->
      if TerminalSet.is_set set term.term_index then (
        if not !first then
          print_string ",";
        print_string " ";
        first := false;

        let name =
          if term.alias <> "" then
            term.alias
          else
            term.tbase.name
        in
        print_string name
      )
    ) terms
  ) else (
    print_string " ..."
  );
  print_string " }"


let print_lr_item env item =
  print_dotted_production item.dprod;
  print_string " [1;30mFirst(.) = ";
  print_terminal_set env.indexed_terms item.lookahead;
  print_string "[0m"


let print_item_set env item_set =
  let print_lr_item item =
    (* print its text *)
    print_string "  ";
    print_lr_item env item;
    print_string "\t\t";

    (* print any transitions on its after-dot symbol *)
    begin match LrItem.symbol_after_dot item with
    | None -> () (* dot is at end *)
    | Some sym ->
        match ItemSet.transition item_set sym with
        | None -> print_string "(no transition)"
        | Some is -> Printf.printf "--> %d" (int_of_state_id is.state_id)
    end;

    print_newline ();
  in

  Printf.printf "ItemSet %d {\n" (int_of_state_id item_set.state_id);
  List.iter print_lr_item (List.sort ~cmp:(fun a b -> compare a b) (item_set.kernel_items @ item_set.nonkernel_items));
  print_newline ();

  (* print transition function directly, since I'm now throwing
   * away items sometimes *)
  Array.iteri (fun i transition ->
    match transition with
    | None -> ()
    | Some transition ->
        Printf.printf "  on terminal %s go to %d\n"
          env.indexed_terms.(i).tbase.name
          (int_of_state_id transition.state_id)
  ) item_set.term_transition;

  Array.iteri (fun i transition ->
    match transition with
    | None -> ()
    | Some transition ->
        Printf.printf "  on nonterminal %s go to %d\n"
          env.indexed_nonterms.(i).nbase.name
          (int_of_state_id transition.state_id)
  ) item_set.nonterm_transition;

  List.iter (fun item ->
    print_string "  can reduce by ";
    PrintGrammar.print_production item.dprod.prod;
    print_newline ()
  ) item_set.dots_at_end;

  print_endline "}";
