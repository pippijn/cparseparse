open Sexplib.Conv

type t = constr
and constr = string * string list
with sexp
