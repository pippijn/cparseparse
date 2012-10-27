open Ast
open Sexplib.Conv

type state_id = int with sexp

type transition_function =
  | Eps (* epsilon transition *)
  | Chr of char (* transition on a character *)
  | Accept of code (* transition from end to start, executing code *)
  with sexp


type state = {
  state_id : state_id;
  outgoing : (transition_function * state_id) list;
} with sexp

type nfa = state IntMap.t with sexp


(* add a new state and return its id *)
let add_state nfa =
  let state_id = IntMap.cardinal nfa in
  let state = { state_id; outgoing = [] } in
  state_id, IntMap.add state_id state nfa

(* replace a state in the NFA *)
let update_state nfa state =
  IntMap.add state.state_id state nfa

(* add transition on c from a to b *)
let add_outgoing nfa a c b =
  let a = IntMap.find a nfa in
  update_state nfa { a with outgoing = (c, b) :: a.outgoing }

(* add new state with transition on c from a to the new state *)
let add_transition nfa a c =
  let next_id, nfa = add_state nfa in
  next_id, add_outgoing nfa a c next_id


let start_state = 0

(* empty automaton with only a start state *)
let empty = IntMap.add start_state { state_id = start_state; outgoing = [] } IntMap.empty


let rec construct_regexp (state_id, nfa) regexp =
  match regexp with
  | Eof ->
      state_id, nfa

  | Char c ->
      (* on a character, simply make a transition to the next state *)
      add_transition nfa state_id (Chr (Sloc.value c))

  | Sequence list ->
      List.fold_left construct_regexp (state_id, nfa) list

  | OrGrouping list ->
      (* in a list of alternatives, make a transition from the current
       * state to all alternatives and a transition to a common target
       * state from each alternative end-state *)
      let end_states, nfa =
        List.fold_left (fun (end_states, nfa) regexp ->
          let end_state, nfa = construct_regexp (state_id, nfa) regexp in
          end_state :: end_states, nfa
        ) ([], nfa) list
      in

      (* make a new state *)
      let common_end, nfa = add_state nfa in

      (* add an epsilon transition from all end states to the common end *)
      let nfa =
        List.fold_left (fun nfa end_state ->
          add_outgoing nfa end_state Eps common_end
        ) nfa end_states
      in

      (* the common end is our new state *)
      common_end, nfa

  | Binding (regexp, name) ->
      construct_regexp (state_id, nfa) regexp

  | Plus (regexp) ->
      (* a+ makes an epsilon transition from the end of 'a' back
       * to this state *)
      let end_state, nfa = construct_regexp (state_id, nfa) regexp in
      end_state, add_outgoing nfa end_state Eps state_id

  | AnyChar | String _ | Lexeme _ | CharClass _ | Question _ | Star _ | Quantified _ ->
      failwith ("unresolved regexp: " ^ Sexplib.Sexp.to_string_hum (sexp_of_regexp regexp))


let construct_rule (Rule (regexp, code)) =
  (* create a local start state for this rule and an epsilon transition
   * from the global start state *)
  let automaton = add_transition empty start_state Eps in

  (* construct one NFA for this rule's regexp *)
  let end_state, nfa = construct_regexp automaton regexp in

  (* make the final accept-transition back to 0 via code *)
  add_outgoing nfa end_state (Accept code) start_state


let construct_lexer (Lexer (name, args, rules)) =
  List.rev_map construct_rule rules


let merge nfa1 nfa2 =
  (* renumber all of nfa2's states except the global start state
   * so that they don't clash with nfa1's states *)
  let local_start = IntMap.cardinal nfa1 in
  let nfa =
    IntMap.fold (fun state_id state nfa ->
      assert (state_id == state.state_id);
      if state_id == start_state then
        nfa
      else
        let state_id = state.state_id + local_start - 1 in
        let outgoing =
          List.rev_map (fun (func, target) ->
            if target == start_state then
              (func, target)
            else
              (func, target + local_start - 1)
          ) state.outgoing
        in
        IntMap.add state_id { state_id; outgoing } nfa
    ) nfa2 nfa1
  in

  (* now add a transition from nfa1's global start state (0)
   * to nfa2's local start state *)
  add_outgoing nfa start_state Eps local_start


let rec follow_epsilon enfa state =
  let enfa, outgoing =
    List.fold_left (fun (enfa, outgoing) (func, target) ->
      match func with
      | Eps ->
          (* follow all epsilon transitions for the target state and add
           * their target to the current state *)
          follow_epsilon enfa (IntMap.find target enfa)
      | _ ->
          (* keep the others *)
          enfa, (func, target) :: outgoing
    ) (enfa, []) state.outgoing
  in

  (* update the state in the eNFA *)
  let enfa = update_state enfa { state with outgoing } in
  enfa, outgoing


let eliminate_epsilon enfa =
  let enfa, nfa =
    IntMap.fold (fun state_id state (enfa, nfa) ->
      (* for each transition in the current state *)
      let enfa, outgoing = follow_epsilon enfa state in
      if outgoing == [] then
        (* the state contained only epsilon transitions,
         * so we drop it entirely *)
        enfa, nfa
      else
        enfa, IntMap.add state_id { state_id; outgoing } nfa
    ) enfa (enfa, empty)
  in

  nfa


let construct (Program (pre, aliases, lexers, post)) =
  assert (aliases == []);
  (* first, construct eNFAs (epsilon NFA) for each lexer *)
  let automata = List.rev_map construct_lexer lexers in

  (* merge all automata into one big eNFA *)
  let nfa = List.fold_left (List.fold_left merge) empty automata in

  (* eliminate epsilon transitions *)
  let nfa = eliminate_epsilon nfa in

  Sexplib.Sexp.output_hum stdout (sexp_of_nfa nfa);
  print_newline ();

  ()
