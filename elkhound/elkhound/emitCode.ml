open Camlp4.PreCast
open GrammarType

module OCamlPrinter = Printers.OCaml


(************************************************
 * :: Output helpers
 ************************************************)


let closing f x channels =
  let r =
    try
      f x
    with e ->
      List.iter close_out channels;
      raise e
  in
  List.iter close_out channels;
  r


(************************************************
 * :: Toplevel code generators/printers
 ************************************************)


let emit_tokens name terms =
  (* Tokens *)
  let dcl = name ^ "Tokens.mli" in
  let out = name ^ "Tokens.ml" in
  let intf, impl = EmitTokens.make_ml_tokens terms in

  OCamlPrinter.print_interf ~output_file:dcl intf;
  OCamlPrinter.print_implem ~output_file:out impl


let emit_parse_tree name prods_by_lhs =
  (* Parse Tree *)
  let out = name ^ "Ptree.ml" in
  let impl = EmitPtree.make_ml_parse_tree prods_by_lhs in

  OCamlPrinter.print_implem ~output_file:out impl;
  (* TODO: with sexp *)
  ignore (Sys.command ("sed -i -e 's/type t = string;;/type t = string with sexp;;/' " ^ out));
  ignore (Sys.command ("sed -i -e 's/ | SEXP;;/ with sexp;;/' " ^ out))


let emit_user_actions name terms nonterms prods final_prod verbatims impl_verbatims =
  (* Actions *)
  let dcl = name ^ "Actions.mli" in
  let out = name ^ "Actions.ml" in
  let intf, impl =
    EmitActions.make_ml_action_code
      terms
      nonterms
      prods
      final_prod verbatims impl_verbatims
  in

  OCamlPrinter.print_interf ~output_file:dcl intf;
  OCamlPrinter.print_implem ~output_file:out impl


let emit_ptree_actions name terms nonterms prods final_prod verbatims impl_verbatims =
  (* Parse Tree Actions *)
  emit_user_actions
      (name ^ "Ptree")
      terms
      (PtreeMaker.nonterms nonterms)
      (PtreeMaker.prods prods)
      final_prod verbatims impl_verbatims


let emit_tables name tables =
  (* Tables *)
  let dcl = name ^ "Tables.mli" in
  let out = name ^ "Tables.ml" in
  let dat = open_out_bin (name ^ "Tables.dat") in

  let intf, impl =
    closing (EmitTables.make_ml_tables dat) tables
      [dat]
  in

  OCamlPrinter.print_interf ~output_file:dcl intf;

  match impl with
  | None ->
      let out = open_out out in
      closing (TablePrinting.print_tables out) tables
        [out]

  | Some impl ->
      OCamlPrinter.print_implem ~output_file:out impl


(************************************************
 * :: Main entry point
 ************************************************)

let emit_ml name terms nonterms prods prods_by_lhs verbatims impl_verbatims tables =
  let final_prod prods = prods.(tables.ParseTablesType.finalProductionIndex) in

  emit_tokens name terms;
  emit_parse_tree name prods_by_lhs;
  emit_user_actions name terms nonterms prods final_prod verbatims impl_verbatims;
  emit_ptree_actions name terms nonterms prods final_prod verbatims impl_verbatims;
  emit_tables name tables
