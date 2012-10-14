open Sexplib.Conv

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                           Definition of AST                           | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

type t = program
and program = definition list
and definition = Ast of string * ast_node list
and ast_node = string * ast_clause list
and ast_clause = node_tag
and node_tag = string list
with sexp

let output_program channel s =
  Sexplib.Sexp.output_hum channel (sexp_of_program s);
  output_char channel '\n'
