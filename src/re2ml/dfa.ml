open Ast
open Sexplib.Conv

module State = struct
  type t = int

  module IntListMap = Hashtbl.Make(struct
    type t = int list

    let hash l = List.fold_left ( * ) 1 l
    let rec equal a b =
      a == b ||
      match a, b with
      | hd1 :: tl1, hd2 :: tl2 -> hd1 == hd2 && equal tl1 tl2
      | [], [] -> true
      | _ -> false
  end)

  type store = {
    states  : int IntListMap.t;
    subsets : int list BatDynArray.t;
  }

  let make_store () = {
    states = IntListMap.create 13;
    subsets = BatDynArray.create ();
  }

  let subset { subsets } id = BatDynArray.get subsets id

  let of_list { states; subsets } subset =
    try
      IntListMap.find states subset
    with Not_found ->
      let id = IntListMap.length states in
      IntListMap.add states subset id;
      BatDynArray.add subsets subset;
      id

  let start store = of_list store [0]
  let to_string store id = String.concat ", " (List.map string_of_int (subset store id))
end

module Transition = struct
  type t = int

  let encode_char c = Char.code c
  let encode_accept action = action + 256

  let decode_char t = Char.chr t
  let decode_accept t = t - 256

  let is_char t = t < 256
  let is_accept t = t >= 256

  let to_string a =
    if a < 256 then
      match Char.chr a with
      | '"' -> "\\\""
      | c -> Char.escaped c
    else
      "A" ^ string_of_int (a - 256)

  let is_final = is_accept
end

module Fsm = Automaton.Imperative.Make(State)(Transition)


(* epsilon closure: a list of epsilon-reachable states for each state *)
type eclosure = Nfa.State.t list Nfa.Map.t with sexp


let uniq estates =
  BatList.sort_unique Nfa.State.compare estates


let get_e_closure eclosure state =
  Nfa.Map.find_default [] state eclosure

(* compute epsilon-accessible states for a state *)
let compute_estates nfa state eclosure =
  let estates =
    List.fold_left (fun estates (func, target) ->
      match func with
      | Nfa.Transition.Eps ->
          (* directly epsilon-accessible states *)
          target :: estates
      | _ ->
          estates
    ) (get_e_closure eclosure state) (Nfa.Fsm.outgoing nfa state)
  in

  (* add this state's estates to the closure *)
  Nfa.Map.add state (uniq estates) eclosure


let get_e_closure eclosure state =
  Nfa.Map.find state eclosure

let e_closure eclosure estates =
  (* add all epsilon-accessible states from other epsilon-accessible
   * states to the e-states list for the current state *)
  List.fold_left (fun estates target ->
    ExtList.unordered_append estates (get_e_closure eclosure target)
  ) estates estates

let rec estates_closure eclosure =
  let closure =
    Nfa.Map.fold (fun state estates closure ->
      let combined_estates = e_closure closure estates in

      (* eliminate duplicates *)
      let combined_estates = uniq combined_estates in

      (* if anything changed *)
      if combined_estates <> estates then
        (* update the map *)
        Nfa.Map.add state combined_estates closure
      else
        (* otherwise, don't change anything *)
        closure
    ) eclosure eclosure
  in

  (* loop until no changes *)
  if closure != eclosure then
    estates_closure closure
  else
    closure


let compute_eclosure nfa =
  (* first step: compute directly epsilon-accessible states *)
  let eclosure = Nfa.Fsm.fold_states (compute_estates nfa) nfa Nfa.Map.empty in

  (* second step: closure *)
  estates_closure eclosure


let next_dfa_state nfa map estate_ids =
  (* clear map *)
  for i = 0 to Array.length map - 1 do
    map.(i) <- [];
  done;

  List.fold_left (fun accept state_id ->
    List.fold_left (fun accept (func, target) ->
      match func with
      | Nfa.Transition.Chr chr ->
          if false then
            Printf.printf "-> %d with %c\n" target chr;

          let chr = Char.code chr in
          map.(chr) <- target :: map.(chr);

          accept

      | Nfa.Transition.Accept action ->
          if false then
            Printf.printf "-> %d [final]\n" target;
          action :: accept

      | Nfa.Transition.Eps ->
          accept

    ) accept (Nfa.Fsm.outgoing nfa state_id)
  ) [] estate_ids


let add_transition store state dfa todo c targets =
  match targets with
  | [] ->
      todo

  | _  ->
      (* each element in the map is a new state in the DFA *)
      let target_state = State.of_list store targets in
      Fsm.add_outgoing dfa state c target_state;
      target_state :: todo


let add_dfa_state store dfa accept map state =
  (* add this subset-state to the automaton *)
  Fsm.add dfa state;

  (* if any state in the epsilon closure is a final state, then
   * this state is also a final state *)
  begin match List.rev accept with
  | [] ->
      (* not a final state *)
      ()
  | [action] ->
      (* a single accept action *)
      Fsm.add_outgoing dfa state (Transition.encode_accept action) (State.start store)
  | action :: _ ->
      (* add the first action *)
      Fsm.add_outgoing dfa state (Transition.encode_accept action) (State.start store)
  end;

  BatArray.fold_lefti (add_transition store state dfa) [] map


let rec construct_subsets nfa store map eclosure dfa state =
  if not (Fsm.mem dfa state) then (
    if false then (
      Printf.printf "--- %s ---\n"
        (State.to_string store state)
    );

    let subset = State.subset store state in

    (* get the epsilon closure for each state in the subset *)
    let estate_ids = e_closure eclosure subset in

    (* eliminate duplicates *)
    let estate_ids = uniq estate_ids in

    (* make a map from input symbol to a set of states *)
    let accept = next_dfa_state nfa map estate_ids in

    let todo = add_dfa_state store dfa accept map state in
    List.iter (construct_subsets nfa store map eclosure dfa) todo
  )


let of_nfa (name, args, (nfa, actions)) =
  if false then (
    Sexplib.Sexp.output_hum stdout (Nfa.Fsm.sexp_of_t nfa);
    print_newline ();
  );

  if Options._dot () then (
    BatStd.with_dispose ~dispose:close_out
      (fun out -> Nfa.Fsm.to_dot out nfa) (open_out "nfa.dot");
    ignore (Sys.command "dot -Tpng nfa.dot -o nfa.png");
  );

  let eclosure = Timing.progress "computing epsilon closure" compute_eclosure nfa in
  if false then (
    Sexplib.Sexp.output_hum stdout (sexp_of_eclosure eclosure);
    print_newline ();
  );

  (* start at the subset containing only the NFA's start state *)
  let dfa =
    let store = State.make_store () in
    let map = Array.make 256 [] in
    let dfa = Fsm.empty store in

    Timing.progress "constructing DFA"
      (Valgrind.Callgrind.instrumented
        (construct_subsets nfa store map eclosure dfa)) (State.start store);

    if Options._dot () then (
      BatStd.with_dispose ~dispose:close_out
        (fun out -> Fsm.to_dot store out dfa) (open_out "dfa.dot");
      ignore (Sys.command "dot -Tpng dfa.dot -o dfa.png");
    );

    dfa
  in

  name, args, (dfa, actions)
