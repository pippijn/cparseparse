open Sexplib

module type Convertible = sig
  include BatSet.OrderedType

  val t_of_sexp : Sexp.t -> t
  val sexp_of_t : t -> Sexp.t
end

module Make(T : Convertible) = struct

  include BatSet.Make(T)

  let t_of_sexp sexp =
    let elements =
      Conv.list_of_sexp T.t_of_sexp sexp
    in
    List.fold_left (fun set elt ->
      add elt set
    ) empty elements

  let sexp_of_t set =
    Conv.sexp_of_list T.sexp_of_t (elements set)

end
