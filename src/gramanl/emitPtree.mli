open Camlp4.PreCast
open GrammarType

val make_ml_parse_tree : production ProdArray.t -> StateId.Production.t list NtArray.t -> StringSet.t -> Ast.str_item
