open Sexplib.Conv

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                           Definition of AST                           | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

type 'a t = 'a program
and 'a program = 'a definition list
and 'a definition =
    Ast of string * ast_node list
  | Map of string * type_decl * 'a rewrite_node list
and ast_node = string * node
and 'a rewrite_node = string * 'a rewrite_clause list
and 'a rewrite_clause = 'a Tree.t * 'a Tree.t
and node =
    CustomNode of ast_clause list
  | NativeNode of string
and type_decl = string list
and ast_clause = topl_tree
and topl_tree = Constr.t
and location = string * tag
and tag = string
and arg = int
with sexp

type untyped_program = unit program
with sexp

open ExtFormat
let f = fprintf

let pp_biarrow_sep pp () = f pp "@ =>@ "

class ['a] print = object (self : 'a)
  method program pp = f pp "@[<v>%a@]@;@." (pp_list self # definition pp_space_sep)
  method definition pp x =
    match x with
    Ast (nm, nodes) ->
      f pp "@[<v>@[<h>ast@ %s@]@;{@[<v 2>@;@[<v>%a@]@]@;}@]@;"
        nm (pp_list self # ast_node pp_space_sep) nodes
  | Map (nm, types, nodes) ->
      f pp "@[<v>@[<h>map@ %s@ %a@]@;{@[<v 2>@;@[<v>%a@]@]@;}@]@;"
        nm self # type_decl types
        (pp_list self # rewrite_node pp_space_sep) nodes
  method ast_node pp (nm, node) = f pp "@[<hov>%s:@ %a@]" nm self # node node
  method rewrite_node pp (nm, clauses) =
    f pp "@[<hov>%s:@ %a@]" nm (pp_list self # rewrite_clause pp_newline_bar_sep) clauses
  method rewrite_clause pp (ltree, rtree) =
    f pp "@[<hov 2>%a@ =>@ %a@]" (new Tree.print) # tree ltree (new Tree.print) # tree rtree
  method node pp = function
    CustomNode clauses -> f pp "%a" (pp_list self # ast_clause pp_newline_bar_sep) clauses
  | NativeNode name -> pp_print_string pp name
  method type_decl pp lst =
    f pp "@[<hov 2>%a@]" (pp_list pp_print_string pp_biarrow_sep) lst
  method ast_clause = self # topl_tree
  method topl_tree = (new Constr.print) # constr
end

let output_program channel s =
  Sexplib.Sexp.output_hum channel (sexp_of_untyped_program s);
  output_char channel '\n'
