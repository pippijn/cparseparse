open GrammarAst
open GrammarType
open AnalysisEnvType

let (|>) = BatPervasives.(|>)


(* This module implements a transformation from AnalysisEnvType types to
 * GrammarAst types. This is the inverse transform of the one performed by
 * GrammarTreeParser and AnalysisEnv. *)


let proddecl_of_prod variant index prod =
  let rhs =
    List.map (function
      | Terminal (tag, term_index) ->
          let { alias; tbase = { name } } = TermArray.get index.terms term_index in
          if alias <> "" then
            RH_string (tag, alias)
          else
            RH_name (tag, name)

      | Nonterminal (tag, nt_index) ->
          let { nbase = { name } } = NtArray.get index.nonterms nt_index in
          RH_name (tag, name)

      (* TODO: RH_prec, RH_forbid *)

    ) prod.right
  in

  let action = BatOption.map CamlAst.string_of_expr (Semantic.action_of_prod variant prod) in

  ProdDecl (PDK_NEW, prod.pbase.name, rhs, action)


let specfunc_of_spec_func funcs = function
  | _, None -> funcs
  | name, Some { params; code; } ->
      SpecFunc (name, params, CamlAst.string_of_expr code) :: funcs


let ast_of_env env variant =
  (* first, we reconstruct the verbatim sections *)
  let verbatims =
    List.map (fun code ->
      TF_verbatim (false, CamlAst.string_of_sig_item code)
    ) (Semantic.verbatims variant env.verbatims)
    @
    List.map (fun code ->
      TF_verbatim (true, CamlAst.string_of_str_item code)
    ) (Semantic.impl_verbatims variant env.verbatims)
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
      TermDecl (Ids.Terminal.to_int term.tbase.index_id, term.tbase.name, term.alias) :: decls
    ) [] env.index.terms
  in
  let types =
    TermArray.fold_left (fun types term ->
      let specfuncs = [
        "dup", Semantic.dup_of_symbol variant term.tbase;
        "del", Semantic.del_of_symbol variant term.tbase;
        "classify", Semantic.classify_of_term variant term;
      ] in

      match Semantic.semtype_of_term variant term with
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
        NtArray.get env.prods_by_lhs nonterm.nbase.index_id
        (* Get actual productions *)
        |> List.map (ProdArray.get env.index.prods)
        (* Transform to ProdDecl *)
        |> List.map (proddecl_of_prod variant env.index)
      in

      let specfuncs = [
        "dup", Semantic.dup_of_symbol variant nonterm.nbase;
        "del", Semantic.del_of_symbol variant nonterm.nbase;
        "merge", Semantic.merge_of_nonterm variant nonterm;
        "keep", Semantic.keep_of_nonterm variant nonterm;
      ] in

      let semtype = Semantic.semtype_of_nonterm variant nonterm in

      let nt =
        TF_nonterm (
          nonterm.nbase.name,
          BatOption.map CamlAst.string_of_ctyp semtype,
          List.fold_left specfunc_of_spec_func [] specfuncs,
          prods,
          (*TODO: subsets*)[]
        ), nonterm.nbase.index_id
      in

      StringMap.add nonterm.nbase.name nt nonterms
    ) StringMap.empty env.index.nonterms
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
