include CompressedBitSet.Make(StateId.Nonterminal)

let t_of_sexp sexp =
  empty

let sexp_of_t bset =
  Sexplib.Sexp.List []
