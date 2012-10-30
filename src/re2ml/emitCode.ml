    (*
    let spans, _, _, _ =
      List.fold_left (fun (spans, goto, lo, hi) (transition, target) ->
        if Dfa.Transition.is_accept transition then (
          let spans =
            if goto != -1 then
              ([lo; hi], goto) :: spans
            else
              spans
          in
          ([transition], target) :: spans, -1, -1, -1
        ) else if goto == -1 then
          spans, target, transition, transition
        else if goto == target && transition == hi + 1 then
          spans, target, lo, transition
        else
          ([lo; hi], target) :: spans, target, transition, transition
      ) ([], -1, -1, -1) outgoing
    in

    let spans_by_target =
      List.fold_left (fun spans_by_target (span, target) ->
        let spans = IntMap.find_default [] target spans_by_target in
        IntMap.add target (span :: spans) spans_by_target
      ) IntMap.empty spans
    in

    let print_span out = function
      | [lo; hi] when lo == hi ->
          Printf.fprintf out "'%s'"
            (Char.escaped (Dfa.Transition.decode_char lo))
      | [lo; hi] ->
          Printf.fprintf out "'%s' .. '%s'"
            (Char.escaped (Dfa.Transition.decode_char lo))
            (Char.escaped (Dfa.Transition.decode_char hi))
      | _ -> ()
    in

    IntMap.iter (fun target spans ->
      if target != 0 then (
        print_string "  ";
        List.iter (Printf.printf "| %a" print_span) spans;
        print_string " -> ";
        match spans with
        | [lo; hi] :: _ ->
            Printf.printf "state_%d (input_char s)\n" target
        | _ -> ()
      )
    ) spans_by_target;

    try
      match IntMap.find 0 spans_by_target with
      | [[action]] ->
          Printf.printf "  | c -> c, (%s)\n"
            (Sloc.value actions.(Dfa.Transition.decode_accept action))
      | _ ->
          failwith "invalid"
    with Not_found ->
      Printf.printf "  | c -> error c\n"
    *)


let emit_automaton (name, args, (dfa, actions)) =
  let args = List.map Sloc.value (name :: args) in

  Printf.printf "let error la = exit 0\n\n";

  Printf.printf "let rec %s la s =" (String.concat " " args);
  print_string "
  let la, action =
    match la with
    | Some la -> state_0 la s
    | None -> state_0 (input_char s) s
  in
  la, match action with
";
  Array.iteri (fun action code ->
    Printf.printf "  | %d -> (%s)\n" action (Sloc.value code);
  ) actions;
  Printf.printf "  | _ -> error la\n";

  Dfa.Fsm.iter (fun state outgoing ->
    Printf.printf "\nand state_%d la s =\n" state;
    Printf.printf "  match la with\n";

    let targets =
      List.fold_left (fun targets (transition, target) ->
        let transitions = IntMap.find_default [] target targets in
        IntMap.add target (transition :: transitions) targets
      ) IntMap.empty outgoing
    in

    IntMap.iter (fun target transitions ->
      if target != 0 then
        print_string "  ";
      List.iter (fun transition ->
        match Dfa.Transition.decode transition with
        | Dfa.Transition.Char c ->
            Printf.printf "| '%s' " (Char.escaped c)
        | _ ->
            ()
      ) (List.rev transitions);

      if target != 0 then
        Printf.printf "->\n      state_%d (input_char s) s\n" target
    ) targets;

    IntMap.iter (fun target transitions ->
      List.iter (fun transition ->
        match Dfa.Transition.decode transition with
        | Dfa.Transition.Accept action ->
            Printf.printf "  | la -> la, %d\n" action
        | _ ->
            ()
      ) transitions;
    ) targets;

    if not (Dfa.Fsm.is_final dfa state) then
      Printf.printf "  | la -> la, -1\n"
  ) dfa;

  print_string "


let rec loop la s =
  let la, _ = token la s in
  loop (Some la) s

let () = loop None stdin
"


let emit pre post dfas =
  List.iter emit_automaton dfas
