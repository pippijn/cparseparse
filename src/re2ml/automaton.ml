open Sexplib.Conv


module Persistent = struct

  module type StateType = sig
    include Sig.OrderedConvertibleType

    (* the local start state for an automaton *)
    val start : t
    (* create a new state *)
    val make : int -> t

    val to_string : t -> string
  end

  module type TransitionType = sig
    include Sig.OrderedConvertibleType

    val is_final : t -> bool
    val to_string : t -> string
  end


  module type S = sig
    type t

    val sexp_of_t : t -> Sexplib.Sexp.t
    val t_of_sexp : Sexplib.Sexp.t -> t

    module S : StateType
    type state = S.t

    module T : TransitionType
    type transition = T.t

    type edge = transition * state with sexp

    val mem : t -> state -> bool

    val add : t -> state -> t
    val add_state : t -> t * state
    val add_outgoing : t -> state -> transition -> state -> t
    val add_transition : t -> state -> transition -> t * state

    val fold_states : (state -> 'a -> 'a) -> t -> 'a -> 'a

    val outgoing : t -> state -> edge list

    val empty : t
    val start : t

    val to_dot : out_channel -> t -> unit
  end


  module Make(S : StateType)(T : TransitionType)
  : S with module S = S
       and module T = T
  = struct

    module S = S
    type state = S.t with sexp

    module T = T
    type transition = T.t with sexp

    module Map = SexpMap.Make(S)
    module Set = SexpSet.Make(S)

    type edge = transition * state with sexp

    type vertex = {
      state_id : S.t;
      outgoing : edge list;
    } with sexp

    type t = {
      size : int;
      states : vertex Map.t;
    } with sexp


    let mem nfa state =
      Map.mem state nfa.states


    (* add a new empty state *)
    let add nfa state_id =
      if mem nfa state_id then
        invalid_arg "can not replace existing state";
      let state = { state_id; outgoing = [] } in
      { states = Map.add state_id state nfa.states; size = nfa.size + 1 }


    (* add a new state and return its id *)
    let add_state nfa =
      let state_id = S.make nfa.size in
      add nfa state_id, state_id


    (* replace a state in the NFA *)
    let update_state nfa state =
      { nfa with states = Map.add state.state_id state nfa.states }


    (* add transition on c from a to b *)
    let add_outgoing nfa a c b =
      let a = Map.find a nfa.states in
      update_state nfa { a with outgoing = (c, b) :: a.outgoing }


    (* add new state with transition on c from a to the new state *)
    let add_transition nfa a c =
      let nfa, next_id = add_state nfa in
      add_outgoing nfa a c next_id, next_id


    let fold_states f nfa x =
      Map.fold (fun state_id state x ->
        f state_id x
      ) nfa.states x


    let outgoing nfa state_id =
      (Map.find state_id nfa.states).outgoing


    (* empty automaton *)
    let empty = { size = 0; states = Map.empty; }
    (* automaton with only a start state *)
    let start = add empty S.start

    let to_dot out nfa =
      output_string out "digraph G {\n";

      let finals =
        Map.fold (fun state_id state finals ->
          if List.exists (fun (func, target) -> T.is_final func) state.outgoing then
            Set.add state_id finals
          else
            finals
        ) nfa.states Set.empty
      in

      output_string out "\tnode [shape = doublecircle];";
      Set.iter (fun final ->
        output_string out " \"";
        output_string out (S.to_string final);
        output_string out "\"";
      ) finals;
      output_string out ";\n\tnode [shape = circle];\n";

      Map.iter (fun source state ->
        List.iter (fun (func, target) ->
          Printf.fprintf out "\t\"%s\" -> \"%s\" [ label = \"%s\" ];\n"
            (S.to_string source)
            (S.to_string target)
            (T.to_string func)
        ) state.outgoing
      ) nfa.states;
      output_string out "}\n"

  end

end


module Imperative = struct

  module type StateType = sig
    include Sig.ConvertibleType

    type store

    val store_of_sexp : Sexplib.Sexp.t -> store
    val sexp_of_store : store -> Sexplib.Sexp.t

    val id_of : store -> t -> int
    val of_id : store -> int -> t
    val invalid : t

    (* the local start state for an automaton *)
    val start : store -> t

    val to_string : store -> t -> string
  end

  module type TransitionType = sig
    include Sig.ConvertibleType

    val id_of : t -> int
    val of_id : int -> t
    val invalid : t

    val is_final : t -> bool
    val to_string : t -> string
  end


  module type S = sig
    include Sig.ConvertibleType

    module S : StateType
    type state = S.t

    module T : TransitionType
    type transition = T.t

    val mem : t -> state -> bool

    val add : t -> state -> unit
    val add_outgoing : t -> state -> transition -> state -> unit

    val fold : (state -> (transition * state) list -> 'a -> 'a) -> t -> 'a -> 'a
    val iter : (state -> (transition * state) list -> unit) -> t -> unit

    val is_final : t -> state -> bool

    val empty : S.store -> t
    val start : S.store -> t

    val to_dot : out_channel -> t -> unit
  end


  module Make(S : StateType)(T : TransitionType)
  : S with module S = S
       and module T = T
  = struct

    module S = S
    type state = S.t with sexp

    module T = T
    type transition = T.t with sexp

    type vertex = {
      state_id : state;
      mutable outgoing : state array;
    } with sexp

    type t = {
      store : S.store;
      mutable states : vertex array;
    } with sexp


    let invalid = {
      state_id = S.invalid;
      outgoing = [||];
    }

    let mem nfa state_id =
      let state_id = S.id_of nfa.store state_id in
      Array.length nfa.states > state_id &&
      nfa.states.(state_id) != invalid


    (* add a new empty state *)
    let add nfa state_id =
      if mem nfa state_id then
        invalid_arg "can not replace existing state";
      let state = { state_id; outgoing = Array.make 128 S.invalid } in
      let state_id = S.id_of nfa.store state_id in
      if Array.length nfa.states <= state_id then
        nfa.states <- Array.init (state_id * 2 + 1) (fun i ->
          if i < Array.length nfa.states then
            nfa.states.(i)
          else
            invalid
        );
      nfa.states.(state_id) <- state


    (* add transition on c from a to b *)
    let add_outgoing nfa a c b =
      let c = T.id_of c in
      let a = nfa.states.(S.id_of nfa.store a) in
      if Array.length a.outgoing <= c then
        a.outgoing <- Array.init (c * 2 + 1) (fun i ->
          if i < Array.length a.outgoing then
            a.outgoing.(i)
          else
            S.invalid
        );
      a.outgoing.(c) <- b


    let outgoing nfa state =
      BatArray.fold_lefti (fun outgoing transition target ->
        if target != S.invalid then
          (T.of_id transition, target) :: outgoing
        else
          outgoing
      ) [] state.outgoing


    let fold f nfa x =
      Array.fold_left (fun x state ->
        if state == invalid then
          x
        else
          f state.state_id (List.rev (outgoing nfa state)) x
      ) x nfa.states


    let iter f nfa =
      Array.iter (fun state ->
        if state != invalid then
          f state.state_id (List.rev (outgoing nfa state))
      ) nfa.states


    (* empty automaton *)
    let empty store = { store; states = Array.make 1 invalid }
    (* automaton with only a start state *)
    let start store = let fsm = empty store in add fsm (S.start store); fsm


    let rec inverted_ranges invs ranges =
      match ranges with
      | (lo1, hi1) :: ((lo2, hi2) :: _ as tl) ->
          let inv = (hi1 + 1, lo2 - 1) in
          inverted_ranges (inv :: invs) tl
      | [] | [(_, _)] ->
          invs


    let string_of_transition c =
      String.escaped (T.to_string (T.of_id c))

    let string_of_range (lo, hi) =
      if lo == hi then
        Printf.sprintf "'%s'" (string_of_transition lo)
      else
        Printf.sprintf "'%s'-'%s'" (string_of_transition lo) (string_of_transition hi)

    let print_ranges is_inv out = function
      | [(lo, hi)] when lo == hi ->
          Printf.fprintf out "'%s'" (string_of_transition lo)
      | ranges ->
          Printf.fprintf out "[%s%s]"
            (if is_inv then "^" else "")
            (String.concat " " (List.rev_map string_of_range ranges))


    let print_ranges out ranges =
      let is_inv =
        let width =
          List.fold_left (fun width (lo, hi) ->
            width + (hi - lo)
          ) 0 ranges
        in

        (* if the ranges cover more than half of the total character
         * range, then this is probably an inverted range *)
        width > 128
      in

      let ranges =
        if is_inv then
          inverted_ranges [] (List.rev ranges)
        else
          ranges
      in

      print_ranges is_inv out ranges


    let is_final_id nfa state =
      let state = nfa.states.(state) in
      state != invalid &&
      BatArray.fold_lefti (fun is_final func target ->
        is_final || (target != S.invalid && T.is_final (T.of_id func))
      ) false state.outgoing


    let is_final nfa state =
      is_final_id nfa (S.id_of nfa.store state)


    let to_dot out nfa =
      output_string out "digraph G {\n";
      output_string out "\trankdir = LR;\n";

      let finals = Array.init (Array.length nfa.states) (is_final_id nfa) in

      output_string out "\tnode [shape = doublecircle];";
      Array.iteri (fun state is_final ->
        if is_final then (
          output_string out " \"";
          output_string out (S.to_string nfa.store (S.of_id nfa.store state));
          output_string out "\"";
        )
      ) finals;
      output_string out ";\n\tnode [shape = circle];\n";

      Array.iter (fun state ->
        if state != invalid then (
          (* construct inverse map: target -> transition list *)
          let outgoing = Array.make (Array.length nfa.states) [] in
          Array.iteri (fun func target ->
            if target != S.invalid then
              let target = S.id_of nfa.store target in
              outgoing.(target) <- func :: outgoing.(target)
          ) state.outgoing;

          Array.iteri (fun target funcs ->
            match List.rev funcs with
            | [] -> ()
            | hd :: tl ->
                let ranges, wip =
                  List.fold_left (fun (ranges, (lo, hi as wip)) func ->
                    if hi + 1 == func then
                      (ranges, (lo, func))
                    else
                      (wip :: ranges, (func, func))
                  ) ([], (hd, hd)) tl
                in

                let ranges = wip :: ranges in

                Printf.fprintf out "\t\"%s\" -> \"%s\" [ label = \"%a\" ];\n"
                  (S.to_string nfa.store state.state_id)
                  (S.to_string nfa.store (S.of_id nfa.store target))
                  print_ranges ranges
          ) outgoing
        )
      ) nfa.states;
      output_string out "}\n";
      flush out

  end

end
