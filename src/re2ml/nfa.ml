open Ast
open Sexplib.Conv

module State = struct
  type t = int with sexp

  let compare a b = a - b
  let make n = n
  let start = make 0
  let to_string = string_of_int
end

module Transition = struct
  (* epsilon-NFA transition functions *)
  type t =
    | Eps (* epsilon transition *)
    | Chr of char (* transition on a character *)
    | Accept of int (* transition from end to start, executing code *)
    with sexp

  let compare = compare
  let to_string = function
    | Eps -> "ε"
    | Chr '"' -> "\\\""
    | Chr c -> Char.escaped c
    | Accept action -> "action " ^ string_of_int action
  let is_final = function
    | Accept _ -> true
    | _ -> false
end

module Map = SexpMap.Make(State)
module Fsm = Automaton.Persistent.Make(State)(Transition)


let rec construct_regexp (nfa, state_id) regexp =
  match regexp with
  | Eof ->
      nfa, state_id

  | Char c ->
      (* on a character, simply make a transition to the next state *)
      Fsm.add_transition nfa state_id (Transition.Chr (Sloc.value c))

  | Sequence seq ->
      List.fold_left construct_regexp (nfa, state_id) seq

  | OrGrouping group ->
      (* in a list of alternatives, make a transition from the current
       * state to all alternatives and a transition to a common target
       * state from each alternative end-state *)
      let nfa, end_states =
        List.fold_left (fun (nfa, end_states) regexp ->
          let nfa, end_state = construct_regexp (nfa, state_id) regexp in
          nfa, end_state :: end_states
        ) (nfa, []) group
      in

      (* make a new state *)
      let nfa, common_end = Fsm.add_state nfa in

      (* add an epsilon transition from all end states to the common end *)
      let nfa =
        List.fold_left (fun nfa end_state ->
          Fsm.add_outgoing nfa end_state Transition.Eps common_end
        ) nfa end_states
      in

      (* the common end is our new state *)
      nfa, common_end

  | Binding (regexp, name) ->
      construct_regexp (nfa, state_id) regexp

  | Plus (regexp) ->
      (* a+ makes an epsilon transition from the end of 'a' back
       * to this state *)
      let nfa, end_state = construct_regexp (nfa, state_id) regexp in
      Fsm.add_outgoing nfa end_state Transition.Eps state_id, end_state

  | AnyChar | String _ | Lexeme _ | CharClass _ | Question _ | Star _ | Quantified _ ->
      failwith ("unresolved regexp: " ^ Sexplib.Sexp.to_string_hum (sexp_of_regexp regexp))


let construct_rule (nfa, actions) (Rule (regexp, code)) =
  (* create a local start state for this rule and an epsilon transition
   * from the global start state *)
  let nfa, start_state = Fsm.add_transition nfa State.start Transition.Eps in

  (* construct one NFA for this rule's regexp *)
  let nfa, end_state = construct_regexp (nfa, start_state) regexp in

  (* make the final accept-transition back to 0 via code *)
  let action = BatDynArray.length actions in
  BatDynArray.add actions code;
  Fsm.add_outgoing nfa end_state (Transition.Accept action) State.start, actions


let construct_lexer (Lexer (name, args, rules)) =
  name, args, List.fold_left construct_rule (Fsm.start, BatDynArray.create ()) rules


let construct (Program (pre, aliases, lexers, post)) =
  assert (aliases == []);
  (* first, construct eNFAs (epsilon NFA) for each lexer *)
  List.rev_map construct_lexer lexers
