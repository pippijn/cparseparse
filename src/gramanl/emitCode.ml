open Glr
open Camlp4.PreCast
open GrammarType

module OCamlPrinter = Printers.OCaml


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


let emit_parse_tree name reachable nonterms prods prods_by_lhs =
  (* Parse Tree *)
  let out = name ^ "Ptree.ml" in
  let impl = EmitPtree.make_ml_parse_tree reachable nonterms prods prods_by_lhs in

  OCamlPrinter.print_implem ~output_file:out impl;
  (* TODO: with sexp *)
  ignore (Sys.command
    ("sed -i -e 's/type t = \\([^;|]*\\);;/type t = \\1 with sexp;;/g;s/ | SEXP;;/ with sexp;;/g' " ^ out))


let emit_treematch name reachable index prods_by_lhs =
  (* Parse Tree *)
  let out = name ^ "Treematch.tm" in
  let impl = EmitTreematch.make_ml_treematch reachable index prods_by_lhs in

  BatStd.with_dispose ~dispose:close_out
    (fun out -> output_string out impl) (open_out out)


let emit_symbol_names name terms nonterms =
  (* Actions *)
  let dcl = name ^ "Names.mli" in
  let out = name ^ "Names.ml" in
  let intf, impl =
    EmitNames.make_ml_descriptions
      terms
      nonterms
  in

  OCamlPrinter.print_interf ~output_file:dcl intf;
  OCamlPrinter.print_implem ~output_file:out impl


let emit_user_actions name prefix terms nonterms prods final_prod verbatims impl_verbatims =
  (* Actions *)
  let dcl = name ^ prefix ^ "Actions.mli" in
  let out = name ^ prefix ^ "Actions.ml" in
  let intf, impl =
    EmitActions.make_ml_action_code
      terms
      nonterms
      prods
      final_prod verbatims impl_verbatims
  in

  OCamlPrinter.print_interf ~output_file:dcl intf;
  OCamlPrinter.print_implem ~output_file:out impl;
  (* TODO: True/False *)
  ignore (Sys.command ("sed -i -e 's/\\.true/.True/;s/\\.false/.False/' " ^ out))


let emit_tables name tables =
  (* Tables *)
  let dcl = name ^ "Tables.mli" in
  let out = name ^ "Tables.ml" in
  let dat = name ^ "Tables.dat" in

  let intf, impl =
    BatStd.with_dispose ~dispose:close_out
      (EmitTables.make_ml_tables tables) (open_out_bin dat)
  in

  OCamlPrinter.print_interf ~output_file:dcl intf;

  match impl with
  | None ->
      BatStd.with_dispose ~dispose:close_out
        (TablePrinting.print_tables tables) (open_out out)

  | Some impl ->
      OCamlPrinter.print_implem ~output_file:out impl


(************************************************
 * :: Main entry point
 ************************************************)

let emit_ml dirname index prods_by_lhs variants reachable tables =
  let open AnalysisEnvType in

  let final_prod = StateId.Production.of_int tables.ParseTablesType.finalProductionIndex in

  let name = dirname ^ "/" ^ String.lowercase (Options._module_prefix ()) in

  emit_tokens name index.terms;
  emit_parse_tree name reachable index.nonterms index.prods prods_by_lhs;
  emit_treematch name reachable index prods_by_lhs;
  emit_symbol_names name index.terms index.nonterms;

  List.iter (fun { prefix; verbatims; impl_verbatims; variant_nonterms; variant_prods } ->
    emit_user_actions name prefix index.terms variant_nonterms variant_prods final_prod verbatims impl_verbatims
  ) variants;

  emit_tables name tables
