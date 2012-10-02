(* parsetables.ml *)
(* representation of parsing tables *)
(* based on elkhound/parsetables.h *)


(* The C++ implementation is designed so the sizes of the various
 * table entries can be adjusted.  I'm reflecting that design here,
 * but I am just using 'int' as the size choice everywhere, since
 * (I think) OCaml arrays don't get smaller if you use (e.g.) char.
 *
 * The way to make array of char efficiently is using strings, but
 * that's a TODO at best, for now.
 *)


(* for action entries; some places may still be called int *)
type tActionEntry = int

(* identifier for a state in the finite automaton *)
type tStateId = int
let cSTATE_INVALID = -1

(* entry in the goto table *)
type tGotoEntry = int

(* index for a terminal *)
type tTermIndex = int

(* index for a nonterminal *)
type tNtIndex = int

(* index for a production *)
type tProdIndex = int

(* ErrorBitsEntry goes here *)


(* encode a symbol in the 'stateSymbol' map *)
(*   N+1:  terminal N *)
(*   0:    no symbol *)
(*   -N-1: nonterminal N *)
type tSymbolId = int
let symIsTerm    id =  id > 0
let symAsTerm    id =  id - 1      (* why not TermIndex? don't know *)
let symIsNonterm id =  id < 0
let symAsNonterm id = -id - 1


(* collection of data needed for the online parsing algorithm *)
type tParseTables = {
  (* grammar counts *)
  numTerms : int;
  numNonterms : int;
  numProds : int;

  (* # of states in LR automaton *)
  numStates : int;

  (* action table, indexed by (state*actionCols + lookahead) *)
  actionCols : int;
  actionTable : tActionEntry array;

  (* goto table, indexed by (state*gotoCols + nontermId) *)
  gotoCols : int;
  gotoTable : tGotoEntry array;

  (* production info, indexed by production id *)
  prodInfo_rhsLen : int array;         (* this is 'unsigned char' array in C++ *)
  prodInfo_lhsIndex : tNtIndex array;

  (* map state to symbol shifted to arrive at that state *)
  stateSymbol : tSymbolId array;

  (* ambiguous actions: one big list, for allocation purposes; then
   * the actions encode indices into this table; the first indexed
   * entry gives the # of actions, and is followed by that many
   * actions, each interpreted the same way ordinary 'actionTable'
   * entries are *)
  ambigTableSize : int;                (* redudant in OCaml... *)
  ambigTable : tActionEntry array;

  (* total order on nonterminals; see elkhound/parsetables.h *)
  nontermOrder : tNtIndex array;

  (* TODO: implement some of the table compression options? *)

  (* start state id (always 0) *)
  startState : tStateId;
  
  (* index of last production to reduce *)
  finalProductionIndex : int;
}


(* -------------- ParseTables client access interface -------------- *)
let getActionEntry tables state tok =
  tables.actionTable.(state * tables.actionCols + tok)

let getActionEntry_noError tables state tok =
  getActionEntry tables state tok


let isShiftAction tables code =
  code > 0 && code <= tables.numStates

(* needs tables for compression *)
let decodeShift code shiftedTerminal =
  code - 1

let isReduceAction code =
  code < 0

(* needs tables for compression *)
let decodeReduce code inState =
  -(code + 1)

let isErrorAction (*tables*) code =
  code = 0

                       
(* this returns an index into the ambigTable *)
(* needs tables for compression *)
let decodeAmbigAction tables code inState =
  code - 1 - tables.numStates


let getGotoEntry tables stateId nontermId =
  tables.gotoTable.(stateId * tables.gotoCols + nontermId)

(* needs tables for compression *)
let decodeGoto code shiftNonterminal =
  code


let getProdInfo_rhsLen tables rule =
  tables.prodInfo_rhsLen.(rule)

let getProdInfo_lhsIndex tables rule =
  tables.prodInfo_lhsIndex.(rule)


let getNontermOrdinal tables idx =
  tables.nontermOrder.(idx)


(* EOF *)
