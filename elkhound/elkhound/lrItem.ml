open AnalysisEnvType

(************************************************************
 * :: Functions
 ************************************************************)


(* this returns a dummy item; it allocates the bitmap for 'lookahead',
 * but those bits and the 'dprod' pointer will be overwritten many
 * times during the algorithm *)
let empty term_count = {
  dprod = empty_dotted_production;
  lookahead = TerminalSet.create term_count;
}


let is_dot_at_start item =
  DottedProduction.is_dot_at_start item.dprod


let is_dot_at_end item =
  DottedProduction.is_dot_at_end item.dprod


let symbol_after_dot item =
  DottedProduction.symbol_after_dot item.dprod


let first_includes sym t =
  let open GrammarType in
  match sym with
  | Terminal (_, term) -> term == t
  | Nonterminal (_, nonterm) ->
      (* this generalises 'isExtendingShift'.. and while this did help
       * eliminate one S/R in a grammar I was working on, there were
       * others that could not be eliminated at all (they were not
       * statically decidable), so this generalisation might not be
       * useful after all *)
      TerminalSet.mem nonterm.first t.term_index


let is_extending_shift item nonterm term =
  let open GrammarType in
  not (is_dot_at_end item) && (* shift *)
  item.dprod.prod.left == nonterm && (* extending nonterm *)
  first_includes (BatOption.get (symbol_after_dot item)) term (* with t *)
