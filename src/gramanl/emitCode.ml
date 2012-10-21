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


let emit_parse_tree name prods prods_by_lhs reachable =
  (* Parse Tree *)
  let out = name ^ "Ptree.ml" in
  let impl = EmitPtree.make_ml_parse_tree prods prods_by_lhs reachable in

  OCamlPrinter.print_implem ~output_file:out impl;
  (* TODO: with sexp *)
  ignore (Sys.command
    ("sed -i -e 's/type t = \\([^;|]*\\);;/type t = \\1 with sexp;;/g;s/ | SEXP;;/ with sexp;;/g' " ^ out))


let emit_treematch name prods prods_by_lhs reachable =
  (* Parse Tree *)
  let out = name ^ "Treematch.tm" in
  let impl = EmitTreematch.make_ml_treematch reachable prods prods_by_lhs in

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
  OCamlPrinter.print_implem ~output_file:out impl;
  (* TODO: True/False *)
  ignore (Sys.command ("sed -i -e 's/\\.true/.True/;s/\\.false/.False/' " ^ out))


let emit_ptree_actions name terms nonterms prods final_prod verbatims impl_verbatims prods_by_lhs reachable =
  (* Parse Tree Actions *)
  emit_user_actions
      (name ^ "Ptree")
      terms
      (PtreeMaker.nonterms reachable nonterms)
      (PtreeMaker.prods reachable prods_by_lhs prods)
      final_prod verbatims impl_verbatims


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

let emit_ml dirname index prods_by_lhs verbatims impl_verbatims tables =
  let open AnalysisEnvType in

  let final_prod = StateId.Production.of_int tables.ParseTablesType.finalProductionIndex in

  let name = dirname ^ "/" ^ String.lowercase (Options._module_prefix ()) in

  let reachable = Reachability.compute_reachable_tagged index.prods prods_by_lhs in

  emit_tokens name index.terms;
  emit_parse_tree name index.prods prods_by_lhs reachable;
  emit_treematch name index.prods prods_by_lhs reachable;
  emit_symbol_names name index.terms index.nonterms;
  emit_user_actions name index.terms index.nonterms index.prods final_prod verbatims impl_verbatims;
  emit_ptree_actions name index.terms index.nonterms index.prods final_prod verbatims impl_verbatims prods_by_lhs reachable;
  emit_tables name tables
