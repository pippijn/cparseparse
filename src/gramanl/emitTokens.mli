open Camlp4.PreCast
open GrammarType

val make_ml_tokens :
  (terminal, Sig.readonly) TermArray.t ->
  Ast.sig_item * Ast.str_item
