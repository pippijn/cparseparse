module type S = sig
  include Hashtbl.S

  open Sexplib

  val t_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a t
  val sexp_of_t : ('a -> Sexp.t) -> 'a t -> Sexp.t

  val of_list : (key * 'a) list -> 'a t
  val find_default : 'a -> key -> 'a t -> 'a
end


module Make(T : Sig.HashedConvertibleType) : S with type key = T.t = struct
  open Sexplib

  include Hashtbl.Make(T)


  let of_list bindings =
    let map = create (List.length bindings) in
    List.iter (fun (key, value) ->
      add map key value
    ) bindings;
    map

  let bindings map =
    fold (fun key value bindings ->
      (key, value) :: bindings
    ) map []


  let t_of_sexp value_of_sexp sexp =
    of_list (Conv.list_of_sexp (Conv.pair_of_sexp T.t_of_sexp value_of_sexp) sexp)

  let sexp_of_t sexp_of_value map =
    Conv.sexp_of_list (Conv.sexp_of_pair T.sexp_of_t sexp_of_value) (bindings map)


  let find_default default key map =
    try
      find map key
    with Not_found ->
      default

end
