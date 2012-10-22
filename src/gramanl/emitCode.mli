open Glr
open Camlp4.PreCast

val emit_ml :
  string ->
  AnalysisEnvType.index ->
  StateId.Production.t list NtArray.t ->
  (
    string *
    Ast.sig_item list *
    Ast.str_item list *
    GrammarType.nonterminal NtArray.t *
    GrammarType.production ProdArray.t
  ) list ->
  StringSet.t ->
  ParseTablesType.t -> unit
