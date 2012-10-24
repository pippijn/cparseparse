open Camlp4.PreCast

val make_ml_parse_tree :
  Sig.readonly NtSet.t ->
  GrammarType.index ->
  (Ids.Production.t list, Sig.readonly) NtArray.t ->
  Ast.str_item
