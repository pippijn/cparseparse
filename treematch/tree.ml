open Sexplib.Conv

type t = tree
and tree =
    Tree of string * t list
  | Var of string
  | Const of const
and const =
    String of string
  | Int of int
with sexp
