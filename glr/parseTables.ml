open ParseTablesType


let symIsTerm    id : bool       =  id > 0
let symAsTerm    id : term_index =  id - 1
let symIsNonterm id : bool       =  id < 0
let symAsNonterm id : nt_index   = -id - 1


(* -------------- ParseTables client access interface -------------- *)
let getActionEntry tables (state : state_id) (tok : term_index) =
  tables.actionTable.(state * tables.actionCols + tok)

let getActionEntry_noError tables (state : state_id) (tok : term_index) =
  getActionEntry tables state tok


let isShiftAction tables (code : action_entry) =
  code > 0 && code <= tables.numStates

(* needs tables for compression *)
let decodeShift (code : action_entry) shiftedTerminal =
  code - 1

let isReduceAction (code : action_entry) =
  code < 0

(* needs tables for compression *)
let decodeReduce (code : action_entry) (in_state : state_id) =
  -(code + 1)

let isErrorAction (*tables*) (code : action_entry) =
  code = 0

                       
(* this returns an index into the ambigTable *)
(* needs tables for compression *)
let decodeAmbigAction tables (code : action_entry) (in_state : state_id) =
  code - 1 - tables.numStates


let getGotoEntry tables (state_id : state_id) (nonterm_id : nt_index) =
  tables.gotoTable.(state_id * tables.gotoCols + nonterm_id)

(* needs tables for compression *)
let decodeGoto (code : action_entry) shiftNonterminal =
  code

let getGoto tables (state_id : state_id) (nonterm_id : nt_index) =
  decodeGoto (getGotoEntry tables state_id nonterm_id) nonterm_id


let getProdInfo_rhsLen tables rule =
  tables.prodInfo_rhsLen.(rule)

let getProdInfo_lhsIndex tables rule =
  tables.prodInfo_lhsIndex.(rule)


let getNontermOrdinal tables idx =
  tables.nontermOrder.(idx)


(* OCaml'ish, but slower interface *)

type action_kind =
  | Shift
  | Reduce
  | Error
  | Ambiguous

type action =
  | ShiftAction of (*new_state:*)state_id
  | ReduceAction of (*production:*)prod_index
  | AmbiguousAction of (*start:*)int
  | ErrorAction


let kind_of_action tables (code : action_entry) =
  if isReduceAction code then
    Reduce
  else if isErrorAction code then
    Error
  else if isShiftAction tables code then
    Shift
  else
    Ambiguous



let getAction tables (state : state_id) (tok : term_index) =
  let code = getActionEntry tables state tok in
  if code < 0 then
    ReduceAction (decodeReduce code state)
  else if code = 0 then
    ErrorAction
  else if code <= tables.numStates then
    ShiftAction (decodeShift code tok)
  else
    AmbiguousAction (decodeAmbigAction tables code state)
