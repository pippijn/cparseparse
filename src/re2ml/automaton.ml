open Sexplib.Conv


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
    let finals =
      Set.iter (fun final ->
        output_char out ' ';
        output_string out (S.to_string final);
      ) finals
    in
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
