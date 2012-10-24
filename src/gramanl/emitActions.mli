open Camlp4.PreCast

val make_ml_action_code :
  GrammarType.index ->
  StateId.Production.t ->
  Ast.sig_item list ->
  Ast.str_item list ->
  Ast.sig_item * Ast.str_item
