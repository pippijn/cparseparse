(*----------------------------------------------------------------------------
                              Literals
  --------------------------------------------------------------------------*)
open Sexplib.Conv

type size = [ `I | `L | `LL | `U | `UL | `ULL ] with sexp

type t =
    Float of Loc.t * string
  | Int of Loc.t * size * string
  | String of Loc.t * string
  | Char of Loc.t * string with sexp
