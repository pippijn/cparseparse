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
          begin match alias with
          | None ->
              RH_name (tag, name)
          | Some alias ->
              RH_string (tag, alias)
          end

      | Nonterminal (tag, nt_index) ->
          let { nbase = { name } } = NtArray.get index.nonterms nt_index in
          RH_name (tag, name)

      (* TODO: RH_prec, RH_forbid *)

    ) prod.right
  in

  let action = BatOption.map CamlAst.loc_string_of_expr (Semantic.action_of_prod variant prod) in

  ProdDecl (PDK_NEW, Some prod.pbase.name, rhs, action)


let specfunc_of_spec_func funcs = function
  | _, None ->
      funcs
  | name, Some { params; code; } ->
      SpecFunc (Sloc.generated name, params, CamlAst.loc_string_of_expr code) :: funcs


let ast_of_env env variant =
  (* first, we reconstruct the verbatim sections *)
  let verbatims =
    List.map (fun code ->
      TF_verbatim (false, CamlAst.loc_string_of_sig_item code)
    ) (Semantic.verbatims variant env.verbatims)
    @
    List.map (fun code ->
      TF_verbatim (true, CamlAst.loc_string_of_str_item code)
    ) (Semantic.impl_verbatims variant env.verbatims)
  in

  (* then, the options *)
  let options = let str = Sloc.generated in [
    TF_option (str "shift_reduce_conflicts", env.options.expectedSR);
    TF_option (str "reduce_reduce_conflicts", env.options.expectedRR);
    TF_option (str "unreachable_nonterminals", env.options.expectedUNRNonterms);
    TF_option (str "unreachable_terminals", env.options.expectedUNRTerms);
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

      let specfuncs =
        List.fold_left specfunc_of_spec_func [] specfuncs
      in

      match Semantic.semtype_of_term variant term with
      | None ->
          assert (specfuncs = []);
          types
      | Some semtype -> 
          TermType (term.tbase.name, CamlAst.loc_string_of_ctyp semtype, (*TODO: specfuncs*)[]) :: types
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

      let specfuncs =
        List.fold_left specfunc_of_spec_func [] specfuncs
      in

      let semtype = Semantic.semtype_of_nonterm variant nonterm in

      let nt =
        TF_nonterm (
          nonterm.nbase.name,
          BatOption.map CamlAst.loc_string_of_ctyp semtype,
          specfuncs,
          prods,
          (*TODO: subsets*)[]
        ), nonterm.nbase.index_id
      in

      StringMap.add (Sloc.value nonterm.nbase.name) nt nonterms
    ) StringMap.empty env.index.nonterms
  in

  let topforms = Merge.({
    verbatims;
    options;
    decls;
    types;
    precs;
    first_nonterm = Sloc.empty_string (* TODO *);
    nonterms;
  }) in

  Merge.to_ast topforms
