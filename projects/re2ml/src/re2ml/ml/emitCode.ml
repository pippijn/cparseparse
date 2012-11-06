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


let make_binding _loc name params code =
  let defn =
    List.fold_right (fun param defn ->
      <:expr<fun $param$ -> $defn$>>
    ) params code
  in

  <:binding<$lid:name$ = $defn$>>


let emit_action_func name params actions =
  let _loc, name = Sloc._loc name in

  let cases =
    BatArray.fold_lefti (fun cases action code ->
      <:match_case<$int:string_of_int action$ -> $CamlAst.expr_of_loc_string code$>>
      :: cases
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

  make_binding _loc (name ^ "_action") params code


let compute_spans acceptance transitions =
  let spans, last =
    List.fold_left (fun (spans, wip) transition ->
      match transition with
      | Dfa.Transition.Accept _ ->
          spans, wip
      | Dfa.Transition.Chr c ->
          let c = Char.code c in
          acceptance.(c) <- true;
          match wip with
          | None ->
              spans, Some (Range (c, c))
          | Some (Range (lo, hi) as wip) ->
              if hi + 1 == c then
                spans, Some (Range (lo, c))
              else
                (wip :: spans), Some (Range (c, c))
    ) ([], None) (List.rev transitions)
  in

  match last with
  | None -> spans
  | Some span -> span :: spans


type containment =
  | Disjunct
  | Contains
  | Is


let span_contains code (Range (lo, hi)) =
  lo <= code && code <= hi


let range_contains code = function
  | [Range (lo, hi)] when lo == code && hi == code ->
      Is
  | spans when List.exists (span_contains code) spans ->
      Contains
  | _ ->
      Disjunct


let break_spans_on code spans =
  List.fold_left (fun spans (Range (lo, hi) as span) ->
    if lo < code && code < hi then
      (* code is between lo and hi *)
      Range (code + 1, hi) :: Range (code, code) :: Range (lo, code - 1) :: spans
    else if lo < code && code == hi then
      (* code is hi and span contains more than just code *)
      Range (code, code) :: Range (lo, code - 1) :: spans
    else if lo == code && code < hi then
      (* code is lo and span contains more than just code *)
      Range (code + 1, hi) :: Range (code, code) :: spans
    else if lo == code && code == hi then
      (* span is exactly code *)
      span :: spans
    else
      failwith "unhandled case"
  ) [] spans
  |> List.rev


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
  |> List.concat, range_contains (Char.code '\n') spans


let add_case _loc cases target (pattern, matches_newline) =
  match pattern with
  | [] ->
      cases
  | _::_ ->
      let pattern = paOrp_of_list pattern in
      let code = <:expr<$lid:"state_" ^ string_of_int target$ (advance lexbuf)>> in

      let case = <:match_case<$pattern$ -> $code$>> in

      let case =
        if not (Options._auto_loc ()) then
          match matches_newline with
          | Is | Disjunct ->
              case
          | Contains ->
              Diagnostics.warning (Sloc.of_loc _loc ())
                "Transition to state %d on a strict superset of ['\\n']
         Source locations may not be tracked correctly in this rule
         Consider using the -auto-loc option" target;
              case
        else
          match matches_newline with
          | Disjunct ->
              (* new-line can not be matched by this pattern *)
              case
          | Contains ->
              <:match_case<($pattern$ as c) ->
                (* new-line can be matched *)
                if c == $chr:"\\n"$ then
                  Lexing.new_line lexbuf;
                $code$
              >>
          | Is ->
              <:match_case<$pattern$ ->
                (* new-line is the only pattern *)
                Lexing.new_line lexbuf;
                $code$
              >>
      in

      case :: cases


let build_cases _loc targets =
  let acceptance = Array.make CharClass.set_end false in

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
      match transition with
      | Dfa.Transition.Accept action ->
          Some <:match_case<_ -> accept lexbuf $int:string_of_int action$>>
      | Dfa.Transition.Chr _ ->
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


let find_action outgoing actions =
  try
    let action = List.find (function
      | Dfa.Transition.Accept _, _ -> true
      | Dfa.Transition.Chr    _, _ -> false
    ) outgoing in
    match action with
    | Dfa.Transition.Accept action, _ ->
        let _loc, _ = Sloc._loc actions.(action) in
        Some _loc
    | _ -> assert false
  with Not_found ->
    None


let emit_automaton (action_funcs, automata) (name, args, (dfa, actions)) =
  let params = List.map CamlAst.patt_of_loc_string args in
  let args   = List.map CamlAst.expr_of_loc_string args in

  let action_func = emit_action_func name params actions in

  let _loc, name = Sloc._loc name in

  let funcs =
    Dfa.Fsm.fold (fun { Dfa.State.id = state } outgoing (_loc, funcs) ->
      let _loc = BatOption.default _loc (find_action outgoing actions) in

      let targets =
        List.fold_left (fun targets (transition, { Dfa.State.id = target }) ->
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

      let func =
        <:binding<
          $lid:"state_" ^ string_of_int state$ lexbuf =
            match curr_char lexbuf $int:string_of_int state$ with
            $cases$
        >>
      in
      
      _loc, func :: funcs
    ) dfa (_loc, [])
    |> snd |> List.rev
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

  let entry_func =
    make_binding _loc name params
      <:expr<
        fun lexbuf ->
          Lexing.(lexbuf.lex_start_pos <- lexbuf.lex_curr_pos);
          let action = $uid:"Dfa_" ^ name$.state_0 lexbuf in
          $action_call$ lexbuf action
       >>
  in

  action_func :: entry_func :: action_funcs,
  automaton :: automata


let emit pre post dfas =
  let (action_funcs, items) = List.fold_left emit_automaton ([], []) dfas in

  let impl =
    let _loc = Loc.ghost in

    let items =
      <:str_item<
        let error lexbuf =
          if lexbuf.Lexing.lex_eof_reached then
            raise End_of_file
          else
            failwith (Lexing.lexeme lexbuf)
        ;;
      >>
      :: items
    in

    let items =
      match pre with
      | None -> items
      | Some pre ->
          CamlAst.str_items_of_loc_string pre
          :: items
    in

    let items =
      <:str_item<let rec $Ast.biAnd_of_list action_funcs$;;>>
      :: items
    in

    let items =
      match post with
      | None -> items
      | Some post ->
          CamlAst.str_items_of_loc_string post
          :: items
    in

    <:str_item<
      let trace_lexing = false;;

      let advance lexbuf =
        let open Lexing in
        if lexbuf.lex_eof_reached then (
          (* on EOF, do nothing (yywrap?) *)
          lexbuf
        ) else (
          lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos + 1;
          lexbuf
        )
      ;;

      let curr_char lexbuf state =
        let open Lexing in
        if lexbuf.lex_eof_reached then (
          $chr:"\\000"$
        ) else (
          if trace_lexing then (
            Printf.printf "state %3d: process char %d (%d-%d / %d) '%s'\n"
              state
              lexbuf.lex_abs_pos
              lexbuf.lex_start_pos
              lexbuf.lex_curr_pos
              lexbuf.lex_buffer_len
              (Char.escaped lexbuf.lex_buffer.[lexbuf.lex_curr_pos]);
          );

          String.unsafe_get lexbuf.lex_buffer lexbuf.lex_curr_pos
        )
      ;;

      let curr_char lexbuf state =
        let open Lexing in
        if lexbuf.lex_curr_pos == lexbuf.lex_buffer_len then (
          if trace_lexing then (
            print_endline "[1;33mrefill[0m";
          );
          lexbuf.refill_buff lexbuf;
        );

        curr_char lexbuf state
      ;;

      let accept lexbuf action =
        let open Lexing in
        if trace_lexing then (
          if lexbuf.lex_eof_reached then (
            Printf.printf "[1;32maccept at eof: %d[0m\n" action;
          ) else (
            Printf.printf "[1;32maccept %d-%d '%s': %d[0m\n"
              lexbuf.lex_start_pos
              (lexbuf.lex_curr_pos - 1)
              (Lexing.lexeme lexbuf)
              action;
          );
        );
        action
      ;;

      let reject lexbuf =
        let open Lexing in
        if trace_lexing then (
          Printf.printf "[1;31mreject at %d[0m\n" lexbuf.lex_curr_pos;
        );
        -1
      ;;

      (* DFA modules and user functions *)
      $Ast.stSem_of_list (List.rev items)$

      (* sample function tokenising the entire lexbuf *)
      (*
      let rec loop lexbuf =
        let t = token lexbuf in
        if trace_lexing then (
          print_endline "getting next token";
          Printf.printf "position %d\n"
            (lexbuf.Lexing.lex_abs_pos);
          print_token t;
        );
        loop lexbuf
      ;;

      let () =
        Printexc.record_backtrace true;
        try
          let open Lexing in
          let lexbuf = from_channel stdin in
          String.fill lexbuf.lex_buffer 0 (String.length lexbuf.lex_buffer) '\255';
          loop lexbuf
        with e ->
          flush stdout;
          Printf.printf "\nException:\n  %s\n" (Printexc.to_string e);
          Printexc.print_backtrace stdout;
      ;;
      *)
    >>
  in

  print_implem "dfa.ml" impl
