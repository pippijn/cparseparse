(* used for the lookahead sets of LR items, and for the First()
 * sets of production RHSs *)

open Sexplib
include BatBitSet

let t_of_sexp sexp =
  (* batteries 2:
  of_list (Conv.list_of_sexp Conv.int_of_sexp sexp)
  *)
  empty ()

let sexp_of_t bset =
  Sexp.List []
