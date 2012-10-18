open Sexplib.Conv

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                          Clause constructor                           | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

type t = constr
and constr = Ident.uident * Ident.uident list
with sexp

open ExtFormat

let f = fprintf

class print = object (self : 'a)
  method constr pp (nm, l) =
    f pp "%a@ %a" Ident.pp_uident nm (pp_list Ident.pp_uident pp_space_sep) l
end

let name = fst
