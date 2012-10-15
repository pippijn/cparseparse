open Sexplib

module type Convertible = sig
  include BatMap.OrderedType

  val t_of_sexp : Sexp.t -> t
  val sexp_of_t : t -> Sexp.t
end

module Make(T : Convertible) = struct

  include BatMap.Make(T)


  let of_list bindings =
    List.fold_left (fun map (key, value) ->
      add key value map
    ) empty bindings


  let t_of_sexp conv sexp =
    of_list (Conv.list_of_sexp (Conv.pair_of_sexp T.t_of_sexp conv) sexp)

  let sexp_of_t conv map =
    Conv.sexp_of_list (Conv.sexp_of_pair T.sexp_of_t conv) (bindings map)


  let find_default default key map =
    try
      find key map
    with Not_found ->
      default

end
