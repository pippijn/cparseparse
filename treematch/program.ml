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
and type_decl = string * string
and ast_clause = topl_tree
and topl_tree = Constr.t
and location = string * tag
and tag = string
and arg = int
with sexp

type untyped_program = unit program
with sexp
type typed_program = string program
with sexp

open ExtFormat
let f = fprintf

let pp_biarrow_sep pp () = f pp "@ =>@ "

class virtual ['a] base_print = object (self)
  method program pp p = f pp "@[<v>%a@]@;@." (pp_list self # definition pp_space_sep) p
  method definition pp (x : 'a definition) =
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
  method node pp = function
    CustomNode clauses -> f pp "%a" (pp_list self # ast_clause pp_newline_bar_sep) clauses
  | NativeNode name -> pp_print_string pp name
  method type_decl pp (s,d) =
    f pp "@[<hov 2>%a@]" (pp_list pp_print_string pp_biarrow_sep) [s;d]
  method ast_clause = self # topl_tree
  method topl_tree = (new Constr.print) # constr
end

class print = object (self : 'a)
  inherit [unit] base_print
  method rewrite_clause pp (ltree, rtree) =
    f pp "@[<hov 2>%a@ =>@ %a@]" (new Tree.print) # tree ltree (new Tree.print) # tree rtree
end

class typed_print = object (self : 'a)
  inherit [string] base_print
  method rewrite_clause pp (ltree, rtree) =
    f pp "@[<hov 2>%a@ =>@ %a@]" (new Tree.typed_print) # tree ltree (new Tree.typed_print) # tree rtree
end
let output_untyped_program channel s =
  Sexplib.Sexp.output_hum channel (sexp_of_untyped_program s);
  output_char channel '\n'

let output_typed_program channel s =
  Sexplib.Sexp.output_hum channel (sexp_of_typed_program s);
  output_char channel '\n'
