open Glr
open Camlp4.PreCast

val emit_ml :
  string ->
  AnalysisEnvType.index ->
  StateId.Production.t list NtArray.t ->
  Ast.sig_item list ->
  Ast.str_item list -> ParseTablesType.t -> unit
