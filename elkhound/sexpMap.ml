open Sexplib

module type Convertible = sig
  include Map.OrderedType

  val t_of_sexp : Sexp.t -> t
  val sexp_of_t : t -> Sexp.t
end

module Make(T : Convertible) = struct

  include Map.Make(T)

  let t_of_sexp conv sexp =
    let bindings =
      Conv.list_of_sexp (Conv.pair_of_sexp T.t_of_sexp conv) sexp
    in
    List.fold_left (fun map (key, value) ->
      add key value map
    ) empty bindings

  let sexp_of_t conv map =
    Conv.sexp_of_list (Conv.sexp_of_pair T.sexp_of_t conv) (bindings map)

end
