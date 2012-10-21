open Glr
open Camlp4.PreCast

val emit_ml :
  string ->
  AnalysisEnvType.index ->
  StateId.Production.t list array ->
  Ast.sig_item list ->
  Ast.str_item list -> ParseTablesType.t -> unit
