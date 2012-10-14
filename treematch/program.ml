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

open ExtFormat

let f = fprintf

class print = object (self : 'a)
  method program pp = f pp "@[<v>%a@]@;@." (pp_list self # definition pp_space_sep)
  method definition pp x =
    match x with
    Ast (nm, nodes) -> f pp "@[<v>@[<h>ast@ %s@]@;{@[<v 2>@;@[<v>%a@]@]@;}@]@;" nm (pp_list self # ast_node pp_space_sep) nodes
  | Map (nm, types, nodes) -> ()
  method ast_node pp (nm, node) = f pp "@[<hov>%s:@ %a@]" nm self # node node
  method rewrite_node () ((nm, clauses) : (unit * unit))  = ()
  method rewrite_clause () ((ltree, rtree) : (unit * unit)) = ()
  method node pp = function
    CustomNode clauses -> f pp "%a" (pp_list self # ast_clause pp_newline_bar_sep) clauses
  | NativeNode name -> pp_print_string pp name
  method type_decl () (lst : unit) = ()
  method ast_clause = self # topl_tree
  method topl_tree = (new Constr.print) # constr
end

let output_program channel s =
  Sexplib.Sexp.output_hum channel (sexp_of_program s);
  output_char channel '\n'
