open Glr
open Camlp4.PreCast

val emit_ml :
  string ->
  GrammarType.index ->
  StateId.Production.t list NtArray.t ->
  AnalysisEnvType.variant list ->
  NtSet.t ->
  ParseTablesType.t -> unit
