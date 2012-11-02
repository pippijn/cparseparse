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

  val fold_outgoing : ('a -> T.t -> state -> 'a) -> 'a -> t -> state -> 'a

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
    mutable states : vertex option array;
  } with sexp


  let mem fsm state_id =
    let state_id = S.id_of fsm.store state_id in
    Array.length fsm.states > state_id &&
    fsm.states.(state_id) != None


  (* add a new empty state *)
  let add fsm state_id =
    if mem fsm state_id then
      invalid_arg "can not replace existing state";
    let state = { state_id; outgoing = Array.make (CharClass.set_end / 2) [] } in
    let state_id = S.id_of fsm.store state_id in

    (* resize array if necessary *)
    if Array.length fsm.states <= state_id then (
      let states = fsm.states in
      fsm.states <- Array.make (state_id * 2 + 1) None;
      Array.blit
        states 0
        fsm.states 0
        (Array.length states)
    );

    fsm.states.(state_id) <- Some state


  let next_state_id fsm =
    try
      (* XXX: this makes adding new states an O(n log n) operation *)
      BatArray.findi BatOption.is_none fsm.states
    with Not_found ->
      Array.length fsm.states


  let add_state fsm =
    let b = S.make fsm.store (next_state_id fsm) in
    add fsm b;
    b


  (* add transition on c from a to b *)
  let add_outgoing fsm a c b =
    let c = T.id_of c in
    let a = BatOption.get (fsm.states.(S.id_of fsm.store a)) in

    (* resize array if necessary *)
    if Array.length a.outgoing <= c then (
      let outgoing = a.outgoing in
      a.outgoing <- Array.make (c * 2 + 1) [];
      Array.blit
        outgoing 0
        a.outgoing 0
        (Array.length outgoing)
    );

    a.outgoing.(c) <- b :: a.outgoing.(c)


  let add_transition fsm a c =
    let b = add_state fsm in
    add_outgoing fsm a c b;
    b


  let outgoing state =
    BatArray.fold_lefti (fun outgoing transition targets ->
      List.fold_right (fun target outgoing -> (T.of_id transition, target) :: outgoing) targets outgoing
    ) [] state.outgoing


  let fold f fsm x =
    Array.fold_left (fun x state ->
      match state with
      | None -> x
      | Some state ->
          f state.state_id (List.rev (outgoing state)) x
    ) x fsm.states


  let iter f fsm =
    Array.iter (function
      | None -> ()
      | Some state ->
          f state.state_id (List.rev (outgoing state))
    ) fsm.states


  let fold_outgoing f x fsm state_id =
    BatArray.fold_lefti (fun x transition targets ->
      List.fold_left (fun x target ->
        f x (T.of_id transition) target
      ) x targets
    ) x (BatOption.get fsm.states.(S.id_of fsm.store state_id)).outgoing


  let outgoing fsm state_id =
    outgoing (BatOption.get (fsm.states.(S.id_of fsm.store state_id)))


  (* empty automaton *)
  let empty store = { store; states = [||] }
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
    match fsm.states.(state) with
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

    let finals = Array.init (Array.length fsm.states) (is_final_id fsm) in

    output_string out "\tnode [shape = doublecircle];";
    Array.iteri (fun state is_final ->
      if is_final then (
        output_string out " \"";
        output_string out (S.to_string fsm.store (S.of_id fsm.store state));
        output_string out "\"";
      )
    ) finals;
    output_string out ";\n\tnode [shape = circle];\n";

    Array.iter (function
      | None -> ()
      | Some state ->
          (* construct inverse map: target -> transition list *)
          let outgoing = Array.make (Array.length fsm.states) [] in
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
