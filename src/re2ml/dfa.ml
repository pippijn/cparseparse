open Ast
open Sexplib.Conv

module State = struct
  type t = int with sexp

  let states = Hashtbl.create 13
  let subsets = Hashtbl.create 13

  let subset id = Hashtbl.find subsets id

  let compare a b = a - b
  let hash a = a
  let equal a b = a == b

  let of_list subset =
    try
      Hashtbl.find states subset
    with Not_found ->
      let id = Hashtbl.length states in
      Hashtbl.add states subset id;
      Hashtbl.add subsets id subset;
      id

  let make id = failwith "cannot make new DFA state"
  let start = of_list [0]
  let to_string id = "{" ^ String.concat ", " (List.map string_of_int (subset id)) ^ "}"
end

module Transition = struct
  type t = int with sexp

  let encode_char c = Char.code c
  let encode_accept actions action =
    BatDynArray.add actions action;
    BatDynArray.length actions + 256

  let decode_char t = Char.chr t
  let decode_accept actions t = BatDynArray.get actions (t - 256)

  let is_char t = t < 256
  let is_accept t = t >= 256

  let to_string a =
    if a < 256 then
      match Char.chr a with
      | '"' -> "\\\""
      | c -> Char.escaped c
    else
      "action " ^ string_of_int (a - 256)

  let is_final = is_accept
end

module Fsm = Automaton.Imperative.Make(State)(Transition)


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


let add_transition state dfa todo c targets =
  match targets with
  | [] ->
      todo

  | _  ->
      (* each element in the map is a new state in the DFA *)
      let target_state = State.of_list targets in
      Fsm.add_outgoing dfa state c target_state;
      target_state :: todo


let add_dfa_state dfa actions accept map state =
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
      Fsm.add_outgoing dfa state (Transition.encode_accept actions action) State.start
  | action :: _ ->
      (* add the first action *)
      Fsm.add_outgoing dfa state (Transition.encode_accept actions action) State.start
  end;

  BatArray.fold_lefti (add_transition state dfa) [] map


let construct_subsets nfa map eclosure dfa actions state =
  if Fsm.mem dfa state then (
    []
  ) else (
    if false then (
      Printf.printf "--- %a ---\n"
        Sexplib.Sexp.output_hum (State.sexp_of_t state)
    );

    let subset = State.subset state in

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
    let accept = next_dfa_state nfa map estate_ids in

    add_dfa_state dfa actions accept map state
  )


let rec construct_dfa nfa map eclosure dfa actions todo =
  if false then
    Printf.printf "constructing for %d subsets\n" (List.length todo);
  let todo =
    List.fold_left (fun todo subset ->
      construct_subsets nfa map eclosure dfa actions subset
      @ todo
    ) [] todo
  in

  match todo with
  | [] -> ()
  | _  -> construct_dfa nfa map eclosure dfa actions todo


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
  let dfa =
    let actions = BatDynArray.create () in
    let map = Array.make 256 [] in
    let dfa = Fsm.empty () in

    Timing.progress "constructing DFA"
      (Valgrind.Callgrind.instrumented
        (construct_dfa nfa map eclosure dfa actions)) [State.start];

    dfa
  in

  if Options._dot () then (
    BatStd.with_dispose ~dispose:close_out
      (fun out -> Fsm.to_dot out dfa) (open_out "dfa.dot");
    ignore (Sys.command "dot -Tpng dfa.dot -o dfa.png");
  );

  name, args, dfa
