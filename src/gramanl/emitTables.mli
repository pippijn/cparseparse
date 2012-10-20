open Glr
open Camlp4.PreCast

val make_ml_tables : ParseTablesType.t -> out_channel -> Ast.sig_item * Ast.str_item option
