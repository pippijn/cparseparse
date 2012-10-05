(* used for the lookahead sets of LR items, and for the First()
 * sets of production RHSs *)

open Sexplib

type t

(* the approach using BatBitSet turned out to be too much of a
 * bottleneck, so now the bitsets are implemented in C++.
 * they are comparable, hashable and serialisable and clean
 * up their memory on garbage collection *)
external create        : int -> t         = "ml_TerminalSet_create"
external copy          : t -> t           = "ml_TerminalSet_copy"
external count         : t -> int         = "ml_TerminalSet_count"
external set           : t -> int -> unit = "ml_TerminalSet_set"
external is_set        : t -> int -> bool = "ml_TerminalSet_is_set"
external unite         : t -> t -> unit   = "ml_TerminalSet_unite"
external differentiate : t -> t -> unit   = "ml_TerminalSet_differentiate"
external merge         : t -> t -> bool   = "ml_TerminalSet_merge"
external clear         : t -> unit        = "ml_TerminalSet_clear"
external assign        : t -> t -> unit   = "ml_TerminalSet_assign"

let empty_set = create 0

let t_of_sexp sexp =
  empty_set

let sexp_of_t bset =
  Sexp.List []
