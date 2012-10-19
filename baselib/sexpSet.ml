module type S = sig
  include BatSet.S
  include Sig.ConvertibleType with type t := t
end

module Make(T : Sig.ConvertibleType) : S with type elt = T.t = struct
  open Sexplib

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
