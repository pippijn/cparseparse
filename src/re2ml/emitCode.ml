open Camlp4.PreCast
module Printer = Printers.OCaml

let (|>) = BatPervasives.(|>)


let print_implem output_file impl =
  Printer.print_implem ~output_file impl


let paOrp_of_list =
  let _loc = Loc.ghost in
  BatList.reduce (fun orp patt ->
    <:patt<$orp$ | $patt$>>
  )


let emit_action_func name actions =
  let _loc, name = Sloc._loc name in

  let cases =
    BatArray.fold_lefti (fun cases action code ->
      <:match_case<$int:string_of_int action$ -> $CamlAst.expr_of_loc_string code$>> :: cases
    ) [] actions
    |> List.rev
    |> Ast.mcOr_of_list
  in

  <:binding<
    $lid:name ^ "_action"$ la action =
      match action with
      $cases$ | _ -> error la
  >>


let emit_automaton (action_funcs, automata) (name, args, (dfa, actions)) =
  let args = List.map Sloc.value (name :: args) in

  let action_func = emit_action_func name actions in

  let _loc, name = Sloc._loc name in

  let funcs =
    Dfa.Fsm.fold (fun state outgoing funcs ->
      let targets =
        List.fold_left (fun targets (transition, target) ->
          let transitions = IntMap.find_default [] target targets in
          IntMap.add target (transition :: transitions) targets
        ) IntMap.empty outgoing
      in

      let cases =
        IntMap.fold (fun target transitions cases ->
          let case =
            List.rev_map (fun transition ->
              match Dfa.Transition.decode transition with
              | Dfa.Transition.Char c ->
                  [ <:patt<$chr:Char.escaped c$>> ]
              | _ ->
                  []
            ) transitions
            |> List.concat
          in

          match case with
          | [] ->
              cases
          | _::_ ->
              let case = paOrp_of_list case in
              <:match_case<$case$ -> $lid:"state_" ^ string_of_int target$ (input_char s) s>> :: cases
        ) targets []
      in

      let cases = Ast.mcOr_of_list (List.rev cases) in

      let default_case =
        IntMap.fold (fun target transitions case ->
          List.fold_left (fun case transition ->
            match Dfa.Transition.decode transition with
            | Dfa.Transition.Accept action ->
                assert (case == None);
                Some <:match_case<la -> la, $int:string_of_int action$>>
            | _ ->
                case
          ) case transitions
        ) targets None
      in

      let default_case =
        (* non-final state returns error on unexpected input *)
        BatOption.default <:match_case<la -> la, -1>> default_case
      in

      <:binding<
        $lid:"state_" ^ string_of_int state$ la s =
          match la with $cases$ | $default_case$
      >> :: funcs
    ) dfa []
    |> List.rev
  in

  let automaton =
    <:str_item<
      module $uid:"Dfa_" ^ name$ = struct
        let rec $Ast.biAnd_of_list funcs$
      end
    >>
  in

  let entry_func = <:binding<
    $lid:name$ la s =
      let la, action =
        match la with
        | Some la -> state_0 la s
        | None -> state_0 (input_char s) s
      in
      la, $lid:name ^ "_action"$ la action
  >> in

  action_func :: entry_func :: action_funcs,
  automaton :: automata


let emit pre post dfas =
  let (action_funcs, items) = List.fold_left emit_automaton ([], []) dfas in

  let impl =
    let _loc = Loc.ghost in

    let items =
      match pre with
      | None -> items
      | Some pre ->
          CamlAst.str_items_of_loc_string pre :: items
    in

    let items = <:str_item<let rec $Ast.biAnd_of_list action_funcs$>> :: items in

    let items =
      match post with
      | None -> items
      | Some post ->
          CamlAst.str_items_of_loc_string post :: items
    in

    <:str_item<
      $Ast.stSem_of_list (List.rev items)$

      let rec loop la s =
        let la, _ = token la s in
        loop (Some la) s

      let () = loop None stdin
    >>
  in

  print_implem "/dev/stdout" impl
