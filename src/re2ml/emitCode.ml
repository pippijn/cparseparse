open Camlp4.PreCast
module Printer = Printers.OCaml

let (|>) = BatPervasives.(|>)


type span =
  | Range of int * int


let print_implem output_file impl =
  Printer.print_implem ~output_file impl


let paOrp_of_list =
  let _loc = Loc.ghost in
  BatList.reduce (fun orp patt ->
    <:patt<$orp$ | $patt$>>
  )


let emit_action_func name params actions =
  let _loc, name = Sloc._loc name in

  let cases =
    BatArray.fold_lefti (fun cases action code ->
      <:match_case<$int:string_of_int action$ -> $CamlAst.expr_of_loc_string code$>> :: cases
    ) [] actions
    |> List.rev
    |> Ast.mcOr_of_list
  in

  let code =
    <:expr<
      fun lexbuf action ->
        match action with
        $cases$ | _ -> error lexbuf
    >>
  in

  let defn =
    List.fold_right (fun param defn ->
      <:expr<fun $param$ -> $defn$>>
    ) params code
  in

  <:binding<$lid:name ^ "_action"$ = $defn$>>


let compute_spans acceptance transitions =
  let spans, last =
    List.fold_left (fun (spans, wip) transition ->
      if not (Dfa.Transition.is_char transition) then
        spans, wip
      else (
        acceptance.(transition) <- true;
        match wip with
        | None ->
            spans, Some (Range (transition, transition))
        | Some (Range (lo, hi) as wip) ->
            if hi + 1 == transition then
              spans, Some (Range (lo, transition))
            else
              (wip :: spans), Some (Range (transition, transition))
      )
    ) ([], None) (List.rev transitions)
  in

  match last with
  | None -> spans
  | Some span -> span :: spans


let build_pattern _loc spans =
  List.rev_map (fun (Range (lo, hi)) ->
    let a = Char.escaped (Char.chr lo) in
    let b = Char.escaped (Char.chr hi) in

    if lo == hi then [
      (* single character *)
      <:patt<$chr:a$>>;
    ] else if lo == hi - 1 then [
      (* only two characters in the range; we produce
       * separate patterns *)
      <:patt<$chr:a$>>;
      <:patt<$chr:b$>>;
    ] else [
      (* a range with three or more characters; we
       * produce a range-pattern *)
      <:patt<$chr:a$ .. $chr:b$>>
    ]
  ) spans
  |> List.concat


let add_case _loc cases target pattern =
  match pattern with
  | [] ->
      cases
  | _::_ ->
      <:match_case<
        $paOrp_of_list pattern$ ->
          $lid:"state_" ^ string_of_int target$ (advance lexbuf)
      >>
      :: cases


let build_cases _loc targets =
  let acceptance = Array.make 256 false in

  let cases =
    IntMap.fold (fun target transitions cases ->
      compute_spans acceptance transitions
      |> build_pattern _loc
      |> add_case _loc cases target
    ) targets []
  in

  (* cases *)
  Ast.mcOr_of_list (List.rev cases),
  (* accepts_full_range *)
  BatArray.for_all BatPervasives.identity acceptance


let accept_case _loc targets =
  IntMap.fold (fun target transitions case ->
    List.fold_left (fun case transition ->
      if Dfa.Transition.is_accept transition then
        let action = Dfa.Transition.decode_accept transition in
          Some <:match_case<_ -> accept lexbuf $int:string_of_int action$>>
      else
        case
    ) case transitions
  ) targets None


let default_case _loc targets accepts_full_range =
  match accept_case _loc targets with
  | None ->
      if accepts_full_range then
        (* no need for a default case; the full character
         * range is covered *)
        None
      else
        (* non-final state returns error on unexpected input *)
        Some <:match_case<_ -> reject lexbuf>>
  | Some _ as accept ->
      accept


let emit_automaton (action_funcs, automata) (name, args, (dfa, actions)) =
  let params = List.map CamlAst.patt_of_loc_string args in
  let args   = List.map CamlAst.expr_of_loc_string args in

  let action_func = emit_action_func name params actions in

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
        let cases, accepts_full_range = build_cases _loc targets in

        match default_case _loc targets accepts_full_range with
        | None -> cases
        | Some default -> <:match_case<$cases$ | $default$>>
      in

      <:binding<
        $lid:"state_" ^ string_of_int state$ lexbuf =
          trace_state lexbuf $int:string_of_int state$;
          match curr_char lexbuf with
          $cases$
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

  let action_call =
    List.fold_left (fun call arg ->
      <:expr<$call$ $arg$>>
    ) <:expr<$lid:name ^ "_action"$>> args
  in

  let entry_func = <:binding<
    $lid:name$ lexbuf =
      Lexing.(lexbuf.lex_start_pos <- lexbuf.lex_curr_pos);
      let action = $uid:"Dfa_" ^ name$.state_0 (advance lexbuf) in
      $action_call$ lexbuf action
  >> in

  action_func :: entry_func :: action_funcs,
  automaton :: automata


let emit pre post dfas =
  let (action_funcs, items) = List.fold_left emit_automaton ([], []) dfas in

  let impl =
    let _loc = Loc.ghost in

    let items =
      <:str_item<
        let error lexbuf = failwith (Lexing.lexeme lexbuf)
      >> :: items
    in

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
      let advance lexbuf =
        let open Lexing in
        if lexbuf.lex_eof_reached then (
          raise End_of_file
        ) else if lexbuf.lex_curr_pos = lexbuf.lex_buffer_len then (
          print_endline "refill";
          lexbuf.refill_buff lexbuf;
        ) else (
          lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos + 1;
        );

        lexbuf
      ;;

      let curr_char lexbuf =
        let open Lexing in
        lexbuf.lex_buffer.[lexbuf.lex_curr_pos]
      ;;

      let accept lexbuf action =
        let open Lexing in
        assert (lexbuf.lex_curr_pos > 0);
        lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - 1;
        action
      ;;

      let reject lexbuf =
        -1
      ;;


      let trace_state lexbuf state =
        Printf.printf "state %d on '%s'\n" state (Char.escaped (curr_char lexbuf))
      ;;

      $Ast.stSem_of_list (List.rev items)$

      let rec loop lexbuf =
        print_endline "getting next token";
        let _ = token lexbuf in
        loop lexbuf

      let () =
        try
          let lexbuf = Lexing.from_channel stdin in
          String.fill lexbuf.Lexing.lex_buffer 0 (String.length lexbuf.Lexing.lex_buffer - 1) '\000';
          loop lexbuf
        with e ->
          flush stdout;
          raise e
    >>
  in

  print_implem "dfa.ml" impl
