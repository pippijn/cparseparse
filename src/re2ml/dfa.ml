open Ast
open Sexplib.Conv

module State = struct
  type t = int list with sexp

  let compare = compare
  let make n = [n]
  let start = make 0
  let to_string s = String.concat ", " (List.map string_of_int s)
end

module Transition = struct
  (* transition function type without epsilon transitions *)
  type t =
    | Chr of char (* transition on a character *)
    | Accept of Ast.code (* transition from end to start, executing action *)
    with sexp

  let compare = compare
  let to_string = function
    | Chr '"' -> "\\\""
    | Chr c -> Char.escaped c
    | Accept action -> String.escaped (Sloc.value action)
  let is_final = function
    | Accept _ -> true
    | _ -> false
end

module Map = SexpMap.Make(State)
module Fsm = Automaton.Make(State)(Transition)

(* epsilon closure: a list of epsilon-reachable states for each state *)
type eclosure = Nfa.State.t list Nfa.Map.t with sexp


let uniq estates =
  BatList.sort_unique Nfa.State.compare estates


let e_closure eclosure state =
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
    ) (e_closure eclosure state) (Nfa.Fsm.outgoing nfa state)
  in

  (* add this state's estates to the closure *)
  Nfa.Map.add state (uniq estates) eclosure


let e_closure eclosure state =
  Nfa.Map.find state eclosure

let rec estates_closure eclosure =
  let closure =
    Nfa.Map.fold (fun state estates closure ->
      let combined_estates =
        (* add all epsilon-accessible states from other epsilon-accessible
         * states to the e-states list for the current state *)
        List.fold_left (fun estates target ->
          estates @ e_closure closure target
        ) estates estates
      in

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


let rec construct_subsets nfa eclosure dfa subset =
  if Fsm.mem dfa subset then (
    dfa
  ) else (
    if false then (
      Printf.printf "--- %a ---\n"
        Sexplib.Sexp.output_hum (sexp_of_list Nfa.State.sexp_of_t subset)
    );

    (* get the epsilon closure for each state in the subset *)
    let estate_ids =
      subset
      @ List.fold_left (fun estate_ids state_id ->
        e_closure eclosure state_id @ estate_ids
      ) [] subset
    in

    (* eliminate duplicates *)
    let estate_ids = uniq estate_ids in

    (* make a map from input symbol to a set of states *)
    let accept, map =
      List.fold_left (fun map state_id ->
        List.fold_left (fun (accept, map) (func, target) ->
          match func with
          | Nfa.Transition.Chr chr ->
              if false then
                Printf.printf "-> %d with %c\n" target chr;
              let targets = CharMap.find_default [] chr map in
              accept, CharMap.add chr (target :: targets) map
          | Nfa.Transition.Accept action ->
              if false then
                Printf.printf "-> %d [final]\n" target;
              action :: accept, map
          | Nfa.Transition.Eps ->
              accept, map
        ) map (Nfa.Fsm.outgoing nfa state_id)
      ) ([], CharMap.empty) estate_ids
    in

    if false then (
      Sexplib.Sexp.output_hum stdout (CharMap.sexp_of_t (sexp_of_list sexp_of_int) map);
      print_newline ();
    );

    (* add this subset to the automaton *)
    let dfa = Fsm.add dfa subset in

    (* each element in the map is a new state in the DFA *)
    let dfa =
      CharMap.fold (fun c targets dfa ->
        let dfa = construct_subsets nfa eclosure dfa targets in
        Fsm.add_outgoing dfa subset (Transition.Chr c) targets
      ) map dfa
    in

    (* if any state in the epsilon closure is a final state, then
     * this state is also a final state *)
    match List.rev accept with
    | [] ->
        (* not a final state *)
        dfa
    | [action] ->
        (* a single accept action *)
        Fsm.add_outgoing dfa subset (Transition.Accept action) State.start
    | action :: actions ->
        (* add the first action *)
        Fsm.add_outgoing dfa subset (Transition.Accept action) State.start
  )


let of_nfa (name, args, nfa) =
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
  (*let dfa = Fsm.empty in*)
  let dfa = Timing.progress "constructing DFA" (construct_subsets nfa eclosure Fsm.empty) State.start in

  if Options._dot () then (
    BatStd.with_dispose ~dispose:close_out
      (fun out -> Fsm.to_dot out dfa) (open_out "dfa.dot");
    ignore (Sys.command "dot -Tpng dfa.dot -o dfa.png");
  );

  name, args, dfa
