open Sexplib.Conv

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                          Clause constructor                           | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

type t = constr
and constr = string * string list
with sexp
