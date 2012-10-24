open GrammarAst
open GrammarType
open AnalysisEnvType

let (|>) = BatPervasives.(|>)


(* This module implements a transformation from AnalysisEnvType types to
 * GrammarAst types. This is the inverse transform of the one performed by
 * GrammarTreeParser and AnalysisEnv. *)


let proddecl_of_prod index prod =
  let rhs =
    List.map (function
      | Terminal (tag, { alias }) when alias <> "" ->
          RH_string (tag, alias)

      | Nonterminal (tag, nt_index) ->
          let { nbase = { name } } = NtArray.get index.nonterms nt_index in
          RH_name (tag, name)

      | Terminal (tag, { tbase = { name } }) ->
          RH_name (tag, name)

      (* TODO: RH_prec, RH_forbid *)

    ) prod.right
  in

  let action = BatOption.map CamlAst.string_of_expr prod.action in

  ProdDecl (PDK_NEW, prod.prod_name, rhs, action)


let specfunc_of_spec_func funcs = function
  | _, None -> funcs
  | name, Some { params; code; } ->
      SpecFunc (name, params, CamlAst.string_of_expr code) :: funcs


let ast_of_env env variant =
  (* first, we reconstruct the verbatim sections *)
  let verbatims =
    List.map (fun code ->
      TF_verbatim (false, CamlAst.string_of_sig_item code)
    ) variant.verbatims
    @
    List.map (fun code ->
      TF_verbatim (true, CamlAst.string_of_str_item code)
    ) variant.impl_verbatims
  in

  (* then, the options *)
  let options = [
    TF_option ("shift_reduce_conflicts", env.options.expectedSR);
    TF_option ("reduce_reduce_conflicts", env.options.expectedRR);
    TF_option ("unreachable_nonterminals", env.options.expectedUNRNonterms);
    TF_option ("unreachable_terminals", env.options.expectedUNRTerms);
  ] in

  (* after that, the terminals *)
  let decls =
    TermArray.fold_left (fun decls term ->
      TermDecl (StateId.Terminal.to_int term.term_index, term.tbase.name, term.alias) :: decls
    ) [] env.index.terms
  in
  let types =
    TermArray.fold_left (fun types term ->
      let specfuncs = [
        "dup", term.tbase.dup;
        "del", term.tbase.del;
        "classify", term.classify;
      ] in

      match term.tbase.semtype with
      | None ->
          types
      | Some semtype -> 
          TermType (term.tbase.name, CamlAst.string_of_ctyp semtype, (*TODO: specfuncs*)[]) :: types
    ) [] env.index.terms
  in
  let precs =
    TermArray.fold_left (fun precs term ->
      match term.precedence with
      | 0    -> precs
      | prec -> PrecSpec (term.associativity, prec, [term.tbase.name]) :: precs
    ) [] env.index.terms
  in

  (* finally, the nonterminals with their productions *)
  let first_nonterm = "" in
  let nonterms =
    NtArray.fold_left (fun nonterms nonterm ->
      let prods =
        (* Get production indices *)
        NtArray.get env.prods_by_lhs nonterm.nt_index
        (* Get actual productions *)
        |> List.map (ProdArray.get variant.variant_prods)
        (* Transform to ProdDecl *)
        |> List.map (proddecl_of_prod env.index)
      in

      let specfuncs = [
        "dup", nonterm.nbase.dup;
        "del", nonterm.nbase.del;
        "merge", nonterm.merge;
        "keep", nonterm.keep;
      ] in

      let nt =
        TF_nonterm (
          nonterm.nbase.name,
          BatOption.map CamlAst.string_of_ctyp nonterm.nbase.semtype,
          List.fold_left specfunc_of_spec_func [] specfuncs,
          prods,
          (*TODO: subsets*)[]
        ), nonterm.nt_index
      in

      StringMap.add nonterm.nbase.name nt nonterms
    ) StringMap.empty variant.variant_nonterms
  in

  let topforms = Merge.({
    verbatims;
    options;
    decls;
    types;
    precs;
    first_nonterm;
    nonterms;
  }) in

  Merge.to_ast topforms
