open Sexplib.Conv

let (=)  : int -> int -> bool = (==)
let (<>) : int -> int -> bool = (!=)


module type StateType = sig
  include Sig.ConvertibleType

  type store

  val store_of_sexp : Sexplib.Sexp.t -> store
  val sexp_of_store : store -> Sexplib.Sexp.t

  val id_of : store -> t -> int
  val of_id : store -> int -> t

  val make : store -> int -> t

  (* the local start state for an automaton *)
  val start : store -> t

  val to_string : store -> t -> string
end

module type TransitionType = sig
  include Sig.ConvertibleType

  val id_of : t -> int
  val of_id : int -> t

  val is_final : t -> bool
  val to_string : t -> string option
end


module type S = sig
  include Sig.ConvertibleType

  module S : StateType
  type state = S.t

  module T : TransitionType
  type transition = T.t

  type edge = transition * state with sexp

  val mem : t -> state -> bool

  val add : t -> state -> unit
  val add_state : t -> state
  val add_outgoing : t -> state -> transition -> state -> unit
  val add_transition : t -> state -> transition -> state

  val outgoing : t -> state -> edge list

  val fold : (state -> edge list -> 'a -> 'a) -> t -> 'a -> 'a
  val iter : (state -> edge list -> unit) -> t -> unit

  val is_final : t -> state -> bool

  val empty : S.store -> t
  val start : S.store -> t

  val to_dot : lr:bool -> ?final:bool -> out_channel -> t -> unit
end


module Make(S : StateType)(T : TransitionType)
: S with module S = S
     and module T = T
= struct

  type 'a dyn_array = 'a BatDynArray.t
  let dyn_array_of_sexp of_sexp sx = BatDynArray.of_array (array_of_sexp of_sexp sx)
  let sexp_of_dyn_array sexp_of ar = sexp_of_array sexp_of (BatDynArray.to_array ar)

  module S = S
  type state = S.t with sexp

  module T = T
  type transition = T.t with sexp

  type edge = transition * state with sexp

  type vertex = {
    state_id : state;
    mutable outgoing : state list array;
  } with sexp

  type t = {
    store : S.store;
    mutable states : vertex option dyn_array;
  } with sexp


  let mem fsm state_id =
    let state_id = S.id_of fsm.store state_id in
    BatDynArray.length fsm.states > state_id &&
    BatDynArray.get fsm.states state_id  != None


  (* add a new empty state *)
  let add fsm state_id =
    if mem fsm state_id then
      invalid_arg "can not replace existing state";
    let state = { state_id; outgoing = Array.make (CharClass.set_end / 2) [] } in
    let state_id = S.id_of fsm.store state_id in
    if state_id == BatDynArray.length fsm.states - 1 then (
      BatDynArray.add fsm.states (Some state)
    ) else (
      while BatDynArray.length fsm.states <= state_id do
        BatDynArray.add fsm.states None
      done;
      BatDynArray.set fsm.states state_id (Some state)
    )


  let next_state_id fsm =
    BatDynArray.length fsm.states


  let add_state fsm =
    let b = S.make fsm.store (next_state_id fsm) in
    add fsm b;
    b


  (* add transition on c from a to b *)
  let add_outgoing fsm a c b =
    let c = T.id_of c in
    let a = BatOption.get (BatDynArray.get fsm.states (S.id_of fsm.store a)) in
    if Array.length a.outgoing <= c then
      a.outgoing <- Array.init (c * 2 + 1) (fun i ->
        if i < Array.length a.outgoing then
          a.outgoing.(i)
        else
          []
      );
    a.outgoing.(c) <- b :: a.outgoing.(c)


  let add_transition fsm a c =
    let b = add_state fsm in
    add_outgoing fsm a c b;
    b


  let outgoing state =
    BatArray.fold_lefti (fun outgoing transition targets ->
      List.map (fun target -> T.of_id transition, target) targets
      @ outgoing
    ) [] state.outgoing


  let fold f fsm x =
    BatDynArray.fold_left (fun x state ->
      match state with
      | None -> x
      | Some state ->
          f state.state_id (List.rev (outgoing state)) x
    ) x fsm.states


  let iter f fsm =
    BatDynArray.iter (function
      | None -> ()
      | Some state ->
          f state.state_id (List.rev (outgoing state))
    ) fsm.states


  let outgoing fsm state_id =
    outgoing (BatOption.get (BatDynArray.get fsm.states (S.id_of fsm.store state_id)))


  (* empty automaton *)
  let empty store = { store; states = BatDynArray.create () }
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
    match T.to_string (T.of_id c) with
    | None -> ""
    | Some s ->
        "'" ^ String.escaped s ^ "'"

  let string_of_range (lo, hi) =
    if lo = hi then
      string_of_transition lo
    else
      string_of_transition lo ^ "-" ^ string_of_transition hi

  let print_ranges is_inv = function
    | [(lo, hi)] when lo = hi ->
        if is_inv then
          "[^" ^ string_of_transition lo ^ "]"
        else
          string_of_transition lo
    | ranges ->
        Printf.sprintf "[%s%s]"
          (if is_inv then "^" else "")
          (String.concat " " (List.rev_map string_of_range ranges))


  let print_ranges ranges =
    let is_inv =
      let width =
        List.fold_left (fun width (lo, hi) ->
          width + (hi - lo)
        ) 0 ranges
      in

      (* if the ranges cover more than half of the total character
       * range, then this is probably an inverted range *)
      width > CharClass.set_end / 2
    in

    let ranges =
      if is_inv then
        inverted_ranges [] (List.rev ranges)
      else
        ranges
    in

    print_ranges is_inv ranges


  let is_final_id fsm state =
    match BatDynArray.get fsm.states state with
    | None -> false
    | Some state ->
        BatArray.fold_lefti (fun is_final func target ->
          is_final || (target != [] && T.is_final (T.of_id func))
        ) false state.outgoing


  let is_final fsm state =
    is_final_id fsm (S.id_of fsm.store state)


  let to_dot ~lr ?(final=true) out fsm =
    output_string out "digraph G {\n";
    if lr then
      output_string out "\trankdir = LR;\n";

    let finals = Array.init (BatDynArray.length fsm.states) (is_final_id fsm) in

    output_string out "\tnode [shape = doublecircle];";
    Array.iteri (fun state is_final ->
      if is_final then (
        output_string out " \"";
        output_string out (S.to_string fsm.store (S.of_id fsm.store state));
        output_string out "\"";
      )
    ) finals;
    output_string out ";\n\tnode [shape = circle];\n";

    BatDynArray.iter (function
      | None -> ()
      | Some state ->
          (* construct inverse map: target -> transition list *)
          let outgoing = Array.make (BatDynArray.length fsm.states) [] in
          Array.iteri (fun func targets ->
            if final || not (T.is_final (T.of_id func)) then
              List.iter (fun target ->
                let target = S.id_of fsm.store target in
                outgoing.(target) <- func :: outgoing.(target)
              ) targets
          ) state.outgoing;

          Array.iteri (fun target funcs ->
            match List.rev funcs with
            | [] -> ()
            | hd :: tl ->
                let ranges, wip =
                  List.fold_left (fun (ranges, (lo, hi as wip)) func ->
                    if hi + 1 = func then
                      (ranges, (lo, func))
                    else
                      (wip :: ranges, (func, func))
                  ) ([], (hd, hd)) tl
                in

                let label =
                  match print_ranges (wip :: ranges) with
                  | "" -> " [ label = \"ε\" ]"
                  | s  -> " [ label = \"" ^ s ^ "\" ]"
                in

                Printf.fprintf out "\t\"%s\" -> \"%s\"%s;\n"
                  (S.to_string fsm.store state.state_id)
                  (S.to_string fsm.store (S.of_id fsm.store target))
                  label
          ) outgoing
    ) fsm.states;
    output_string out "}\n";
    flush out

end
