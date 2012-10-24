open Glr
open Camlp4.PreCast

val emit_ml :
  string ->
  AnalysisEnvType.index ->
  StateId.Production.t list NtArray.t ->
  AnalysisEnvType.variant list ->
  StringSet.t ->
  ParseTablesType.t -> unit
