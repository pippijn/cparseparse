module type S = sig
  include BatSet.S
  include Sig.OrderedConvertibleType with type t := t

  val of_list : elt list -> t
end

module Make(T : Sig.OrderedConvertibleType) : S with type elt = T.t = struct
  open Sexplib

  include BatSet.Make(T)


  let of_list elts =
    List.fold_left (fun map elt ->
      add elt map
    ) empty elts


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
