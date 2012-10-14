open Sexplib.Conv

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                           Definition of AST                           | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

type t = program
and program = definition list
and definition = Ast of string * ast_node list
and ast_node = string * node
and node =
    CustomNode of ast_clause list
  | NativeNode of string
and ast_clause = topl_tree
and topl_tree = Constr.t
with sexp

let output_program channel s =
  Sexplib.Sexp.output_hum channel (sexp_of_program s);
  output_char channel '\n'
