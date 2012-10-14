open Camlp4.PreCast
open GrammarType

val make_ml_action_code :
  terminal array ->
  nonterminal array ->
  production list array ->
  (production array -> production) ->
  Ast.sig_item list ->
  Ast.str_item list ->
  Ast.sig_item * Ast.str_item
