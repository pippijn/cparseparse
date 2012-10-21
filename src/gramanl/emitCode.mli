open Glr
open Camlp4.PreCast
open GrammarType

val emit_ml :
  string ->
  terminal array ->
  nonterminal array ->
  production array ->
  StateId.Production.t list array ->
  Ast.sig_item list ->
  Ast.str_item list -> ParseTablesType.t -> unit
