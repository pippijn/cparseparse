(* used for the lookahead sets of LR items, and for the First()
 * sets of production RHSs *)

open Sexplib
include BatBitSet


let empty_set = empty ()


let t_of_sexp sexp =
  (* batteries 2:
  of_list (Conv.list_of_sexp Conv.int_of_sexp sexp)
  *)
  empty ()

let sexp_of_t bset =
  Sexp.List []


let merge a b =
  (* check whether a will change by uniting it with b *)
  if not (equals a (union a b)) then (
    unite a b;
    true
  ) else (
    false
  )


let clear set =
  (* clear the set by intersecting it with 0 *)
  intersect set empty_set


let assign a b =
  clear a;
  unite a b
