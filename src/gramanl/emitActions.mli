open Camlp4.PreCast
open GrammarType

val make_ml_action_code :
  terminal TermArray.t ->
  nonterminal NtArray.t ->
  production ProdArray.t ->
  StateId.Production.t ->
  Ast.sig_item list ->
  Ast.str_item list ->
  Ast.sig_item * Ast.str_item
