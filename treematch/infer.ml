module T = Tree

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                  Define operation to transform nodes                  | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

type t = location * operation
and operation
    Remove of arg
  | Replace of arg * arg
  | Subst of arg * node
and location = node * tag
and node = tag = string
and arg = int
