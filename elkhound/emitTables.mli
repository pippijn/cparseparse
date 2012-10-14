open Camlp4.PreCast

val make_ml_tables : out_channel -> ParseTablesType.t -> Ast.sig_item * Ast.str_item option
