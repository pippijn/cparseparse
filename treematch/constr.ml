open Sexplib.Conv

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                          Clause constructor                           | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

type t = constr
and constr = string * string list
with sexp

open ExtFormat

let f = fprintf

class print = object (self : 'a)
  method constr pp (nm, l) =
    f pp "%s@ %a" nm (pp_list pp_print_string pp_space_sep) l
end
