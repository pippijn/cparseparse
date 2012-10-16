open Sexplib.Conv

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                            Tree definition                            | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

type 'a t = 'a tree
and 'a tree =
    Tree of 'a * (string * 'a t list)
  | Var of 'a * string
  | Const of const
and const =
    String of string
  | Int of int
with sexp

open ExtFormat
let f = fprintf

class ['a] print = object (self : 'a)
  method tree pp = function
  | Tree (_, (nm, lst)) -> f pp "@[<hov>(%s@ @[<hov 2>%a@])@]" nm (pp_list self # tree pp_space_sep) lst
  | Var (_,nm) -> pp_print_string pp nm
  | Const c -> f pp "%a" self#const c
  method const pp = function
  | Int i -> f pp "%d" i
  | String s -> f pp "\"%s\"" s
end
