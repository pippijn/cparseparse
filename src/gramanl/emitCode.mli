open Glr
open Camlp4.PreCast

val emit_ml :
  string ->
  GrammarType.index ->
  (StateId.Production.t list, Sig.readonly) NtArray.t ->
  AnalysisEnvType.variant list ->
  Sig.readonly NtSet.t ->
  ParseTablesType.t -> unit
