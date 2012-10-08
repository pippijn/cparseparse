open Sexplib.Conv


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
  first_set                     : TerminalSet.t;

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
  lookahead                     : TerminalSet.t; (* lookahead symbols *)
} with sexp


module DottedProductionS = struct

  type t = dotted_production

  let hash a =
    a.dprod_id

  let compare a b =
    a.dprod_id - b.dprod_id

  let equal a b =
    a == b

  let sexp_of_t = sexp_of_dotted_production
  let t_of_sexp = dotted_production_of_sexp

end

module DottedProductionTable = BatHashtbl.Make(DottedProductionS)
module DottedProductionMap = SexpMap.Make(DottedProductionS)
module DottedProductionSet = SexpSet.Make(DottedProductionS)
module DottedProductionStack = HashStack.Make(DottedProductionTable)


module LrItemS = struct

  type t = lr_item

  let hash a =
    DottedProductionS.hash a.dprod

  let compare a b =
    DottedProductionS.compare a.dprod b.dprod

  let equal a b =
    DottedProductionS.equal a.dprod b.dprod

  let sexp_of_t = sexp_of_lr_item
  let t_of_sexp = lr_item_of_sexp

end

module LrItemTable = BatHashtbl.Make(LrItemS)
module LrItemMap = SexpMap.Make(LrItemS)
module LrItemSet = SexpSet.Make(LrItemS)
module LrItemStack = HashStack.Make(LrItemTable)


(************************************************************
 * :: ItemSet types
 ************************************************************)


type state_id

let int_of_state_id : state_id -> int = Obj.magic
let state_id_of_int : int -> state_id = Obj.magic

let state_id_of_sexp sexp = state_id_of_int (int_of_sexp sexp)
let sexp_of_state_id id   = sexp_of_int (int_of_state_id id)

(* a set of dotted productions, and the transitions between
 * item sets, as in LR(0) set-of-items construction *)
type item_set = {
  (* profiler also reports I'm still spending time comparing item sets; this
   * stores a hash of the numerically sorted kernel item pointer addresses,
   * concatenated into a buffer of sufficient size *)
  (*mutable hash : int;*)

  (* kernel items: the items that define the set; except for
   * the special case of the initial item in the initial state,
   * the kernel items are distinguished by having the dot *not*
   * at the left edge *)
  kernel_items                  : lr_item list;
  (* nonkernel items: those derived as the closure of the kernel
   * items by expanding symbols to the right of dots; here I am
   * making the choice to materialize them, rather than derive
   * them on the spot as needed (and may change this decision) *)
  mutable nonkernel_items       : lr_item list;
  (* transition function (where we go on shifts); None means no transition
   *   Map : (Terminal id or Nonterminal id) -> item_set *)
  term_transition               : item_set option array;
  nonterm_transition            : item_set option array;

  (* profiler reports I'm spending significant time rifling through
   * the items looking for those that have the dot at the end; so this
   * array will point to all such items *)
  mutable dots_at_end           : lr_item list;

  (* need to store this, because I can't compute it once I throw
   * away the items *)
  mutable state_symbol          : GrammarType.symbol option;

  (* numerical state id, should be unique among item sets
   * in a particular grammar's sets *)
  mutable state_id              : state_id;

  (* it's useful to have a BFS tree superimposed on the transition
   * graph; for example, it makes it easy to generate sample inputs
   * for each state.  so we store the parent pointer; we can derive
   * child pointers by looking at all outgoing transitions, and
   * filtering for those whose targets' parent pointers equal 'this'.
   * the start state's parent is None, since it is the root of the
   * BFS tree *)
  mutable bfs_parent            : item_set option;

  mutable hash : int;
} with sexp


module ItemSetS = struct

  type t = item_set

  let hash_kernel_items kernel_items =
    List.fold_left (lxor) 0 (List.map LrItemS.hash kernel_items)

  let hash a =
    a.hash

  (* since nonkernel items are entirely determined by kernel
   * items, and kernel items are sorted, it's sufficient to
   * check for kernel list equality *)
  let rec compare_kernel_items result a b =
    match a, b with
    | ah :: at, bh :: bt ->
        if result <> 0 then
          result
        else
          compare_kernel_items (LrItemS.compare ah bh) at bt
    | _ :: _, [] ->
        1
    | [], _ :: _ ->
        -1
    | [], [] ->
        result

  let compare a b =
    compare_kernel_items 0 a.kernel_items b.kernel_items

  let equal a b =
    if a.hash <> b.hash then
      false
    else
      compare a b = 0

  let sexp_of_t = sexp_of_item_set
  let t_of_sexp = item_set_of_sexp

end

module ItemSetTable = BatHashtbl.Make(ItemSetS)
module ItemSetMap = SexpMap.Make(ItemSetS)
module ItemSetSet = SexpSet.Make(ItemSetS)
module ItemSetStack = HashStack.Make(ItemSetTable)


(************************************************************
 * :: AnalysisEnv types
 ************************************************************)


type env = {
  (* index the symbols on their integer ids *)
  indexed_nonterms              : GrammarType.nonterminal array; (* nt_index -> nonterminal *)
  indexed_terms                 : GrammarType.terminal array; (* term_index -> terminal *)
  indexed_prods                 : GrammarType.production array; (* prod_index -> production *)

  (* during item set closure, profiling reports we spend a lot of time
   * walking the list of productions looking for those that have a given
   * symbol on the LHS; so let's index produtions by LHS symbol index;
   * this array maps each nonterminal to the list of productions with
   * that nonterminal on the LHS *)
  prods_by_lhs                  : GrammarType.production list array;
  
  (* map of production x dot_position -> dotted_production;
   * each element of the 'dotted_prods' array is a pointer to an
   * array of dotted_production objects *)
  dotted_prods                  : dotted_production array array;
  
  (* if entry i,j is true, then nonterminal i can derive nonterminal j
   * (this is a graph, represented (for now) as an adjacency matrix) *)
  derivable                     : Bit2d.t;
  
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
} with sexp



let empty_dotted_production = {
  dprod_id = -1;
  prod = GrammarType.empty_production;
  dot = -1;
  after_dot = None;
  first_set = TerminalSet.empty_set;
  can_derive_empty = false;
  back_pointer = None;
}
