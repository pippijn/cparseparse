open Camlp4.PreCast
open GrammarType

val make_ml_parse_tree :
  NtSet.t ->
  nonterminal NtArray.t ->
  production ProdArray.t ->
  StateId.Production.t list NtArray.t ->
  Ast.str_item
