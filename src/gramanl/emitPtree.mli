open Camlp4.PreCast
open GrammarType

val make_ml_parse_tree :
  Sig.readonly NtSet.t ->
  (nonterminal, Sig.readonly) NtArray.t ->
  (production, Sig.readonly) ProdArray.t ->
  (StateId.Production.t list, Sig.readonly) NtArray.t ->
  Ast.str_item
