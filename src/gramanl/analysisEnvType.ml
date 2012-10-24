open Sexplib.Conv

module Derivable = Bit2d.Make(StateId.Nonterminal)


(************************************************************
 * :: LrItem types
 ************************************************************)


(* a production, with an indicator that says how much of this
 * production has been matched by some part of the input string
 * (exactly which part of the input depends on where this appears
 * in the algorithm's data structures) *)
type dotted_production = {
  prod                          : GrammarType.production; (* the base production *)
  dot                           : int; (* 0 means it's before all RHS symbols, 1 means after first, etc. *)

  (* performance optimization: None if dot at end, or else pointer
   * to the symbol right after the dot *)
  after_dot                     : GrammarType.symbol option;

  (* First of the sentential form that follows the dot; this set
   * is computed by FirstSets.compute_dprod_firsts *)
  mutable first_set             : TerminalSet.t;

  (* also computed by compute_dprod_firsts, this is true if the
   * sentential form can derive epsilon (the empty string) *)
  mutable can_derive_empty      : bool;

  (* during item set closure, I need a way to map from dotted prods to
   * the items which use them; so rather than use a hash table, I'll
   * just annotate the dprods themselves with backpointers; these
   * backpointers *must* be maintained as None when there's no
   * association *)
  mutable back_pointer          : lr_item option;

  (* unique identifier *)
  dprod_id                      : int;
}


(* a dotted production with a lookahead; whereas each production
 * has a fixed number of dotted versions of that production, there
 * can be lots of items, because of the differing lookahead sets
 * (I prefer the name "LRItem" to simply "Item" because the latter
 * easily collides with other uses) *)
and lr_item = {
  mutable dprod                 : dotted_production; (* production and dot position *)
  mutable lookahead             : TerminalSet.t; (* lookahead symbols *)
} with sexp


(************************************************************
 * :: ItemList types
 ************************************************************)

type item_list = {
  items : lr_item list;

  (* profiler also reports I'm still spending time comparing item sets; this
   * stores a hash of the numerically sorted kernel item pointer addresses,
   * concatenated into a buffer of sufficient size *)
  mutable hash : int;
} with sexp


(************************************************************
 * :: ItemSet types
 ************************************************************)


(* a set of dotted productions, and the transitions between
 * item sets, as in LR(0) set-of-items construction *)
type item_set = {
  (* kernel items: the items that define the set; except for
   * the special case of the initial item in the initial state,
   * the kernel items are distinguished by having the dot *not*
   * at the left edge *)
  kernel_items                  : item_list;
  (* nonkernel items: those derived as the closure of the kernel
   * items by expanding symbols to the right of dots; here I am
   * making the choice to materialize them, rather than derive
   * them on the spot as needed (and may change this decision) *)
  mutable nonkernel_items       : lr_item list;
  (* transition function (where we go on shifts); None means no transition
   *   Map : (Terminal id or Nonterminal id) -> item_set *)
  term_transition               : item_set option TermArray.t;
  nonterm_transition            : item_set option NtArray.t;

  (* profiler reports I'm spending significant time rifling through
   * the items looking for those that have the dot at the end; so this
   * array will point to all such items *)
  mutable dots_at_end           : lr_item list;

  (* need to store this, because I can't compute it once I throw
   * away the items *)
  mutable state_symbol          : GrammarType.symbol option;

  (* numerical state id, should be unique among item sets
   * in a particular grammar's sets *)
  mutable state_id              : StateId.State.t;

  (* it's useful to have a BFS tree superimposed on the transition
   * graph; for example, it makes it easy to generate sample inputs
   * for each state.  so we store the parent pointer; we can derive
   * child pointers by looking at all outgoing transitions, and
   * filtering for those whose targets' parent pointers equal 'this'.
   * the start state's parent is None, since it is the root of the
   * BFS tree *)
  mutable bfs_parent            : item_set option;
} with sexp


(************************************************************
 * :: AnalysisEnv types
 ************************************************************)

type index = {
  terms                         : GrammarType.terminal TermArray.t;   (* term_index -> terminal    *)
  nonterms                      : GrammarType.nonterminal NtArray.t;  (* nt_index   -> nonterminal *)
  prods                         : GrammarType.production ProdArray.t; (* prod_index -> production  *)
} with sexp

type variant = {
  prefix			: string;
  variant_nonterms              : GrammarType.nonterminal NtArray.t;  (* nt_index   -> nonterminal *)
  variant_prods                 : GrammarType.production ProdArray.t; (* prod_index -> production  *)
  verbatims                     : CamlAst.sig_item list;
  impl_verbatims                : CamlAst.str_item list;
} with sexp

type env = {
  (* index the symbols on their integer ids *)
  index                         : index;

  (* during item set closure, profiling reports we spend a lot of time
   * walking the list of productions looking for those that have a given
   * symbol on the LHS; so let's index produtions by LHS symbol index;
   * this array maps each nonterminal to the list of productions with
   * that nonterminal on the LHS *)
  prods_by_lhs                  : StateId.Production.t list NtArray.t;

  (* map of production x dot_position -> dotted_production;
   * each element of the 'dotted_prods' array is a pointer to an
   * array of dotted_production objects *)
  dotted_prods                  : dotted_production array ProdArray.t;

  (* if entry i,j is true, then nonterminal i can derive nonterminal j
   * (this is a graph, represented (for now) as an adjacency matrix) *)
  derivable                     : Derivable.t;

  (* true if any nonterminal can derive itself (with no extra symbols
   * surrounding it) in 1 or more steps *)
  mutable cyclic_grammar        : bool;

  (* distinguished start state; NOTE: much of the grammar analysis
   * code currently assumes (and checks) that state 0 is the start
   * state, so if you want to do something different, that code might
   * need to be changed *)
  mutable start_state           : item_set option;

  (* options from grammar *)
  options                       : GrammarType.config;
  variants			: variant list;
} with sexp
