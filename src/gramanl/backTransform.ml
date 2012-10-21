open GrammarAst
open GrammarType
open AnalysisEnvType

(* This module implements a transformation from AnalysisEnvType types to
 * GrammarAst types. This is the inverse transform of the one performed by
 * GrammarTreeParser and AnalysisEnv. *)

let ast_of_env env =
  (* first, we reconstruct the verbatim sections *)
  let verbatims =
    List.map (fun code ->
      TF_verbatim (false, CamlAst.string_of_sig_item code)
    ) env.verbatims
    @
    List.map (fun code ->
      TF_verbatim (true, CamlAst.string_of_str_item code)
    ) env.impl_verbatims
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
      match term.tbase.semtype with
      | None ->
          types
      | Some semtype -> 
          TermType (term.tbase.name, CamlAst.string_of_ctyp semtype, []) :: types
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
  let nonterms = StringMap.empty in

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
