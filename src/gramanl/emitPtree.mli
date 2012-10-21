open Camlp4.PreCast
open GrammarType

val make_ml_parse_tree : production array -> StateId.Production.t list array -> Ast.str_item * StringSet.t
