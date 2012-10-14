open Sexplib

type t

(* the approach using BatBitSet turned out to be too much of a
 * bottleneck, so now the bitsets are implemented in C++.
 * they are comparable, hashable and serialisable and clean
 * up their memory on garbage collection *)
external create        : int -> t         = "ml_BitSet_create"
external copy          : t -> t           = "ml_BitSet_copy"
external cardinal      : t -> int         = "ml_BitSet_count"
external add           : t -> int -> unit = "ml_BitSet_set"
external mem           : t -> int -> bool = "ml_BitSet_is_set"
external unite         : t -> t -> unit   = "ml_BitSet_unite"
external differentiate : t -> t -> unit   = "ml_BitSet_differentiate"
external merge         : t -> t -> bool   = "ml_BitSet_merge"
external clear         : t -> unit        = "ml_BitSet_clear"
external assign        : t -> t -> unit   = "ml_BitSet_assign"

let empty_set = create 0

let t_of_sexp sexp =
  empty_set

let sexp_of_t bset =
  Sexp.List []
