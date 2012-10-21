open Camlp4.PreCast
open GrammarType

val make_ml_parse_tree : production array -> StateId.Production.t list array -> StringSet.t -> Ast.str_item
