(* used for the lookahead sets of LR items, and for the First()
 * sets of production RHSs *)

include CompressedBitSet

let t_of_sexp sexp =
  empty

let sexp_of_t bset =
  Sexplib.Sexp.List []
