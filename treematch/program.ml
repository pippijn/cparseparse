open Sexplib.Conv

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                           Definition of AST                           | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

type t = program
and program = definition list
and definition =
    Ast of string * ast_node list
  | Map of string * type_decl * rewrite_node list
and ast_node = string * node
and rewrite_node = string * rewrite_clause list
and rewrite_clause = Tree.t * Tree.t
and node =
    CustomNode of ast_clause list
  | NativeNode of string
and type_decl = string list
and ast_clause = topl_tree
and topl_tree = Constr.t
with sexp

let output_program channel s =
  Sexplib.Sexp.output_hum channel (sexp_of_program s);
  output_char channel '\n'
