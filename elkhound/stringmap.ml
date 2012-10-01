module M = struct
  include Map.Make (String)
  open Sexplib

  let t_of_sexp conv sexp =
    let bindings =
      Conv.list_of_sexp (Conv.pair_of_sexp Conv.string_of_sexp conv) sexp
    in
    List.fold_left (fun map (key, value) ->
      add key value map
    ) empty bindings

  let sexp_of_t conv value =
    Conv.sexp_of_list (Conv.sexp_of_pair Conv.sexp_of_string conv) (bindings value)

end
