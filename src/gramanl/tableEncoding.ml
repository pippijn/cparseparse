open Glr
open ParseTablesType
open AnalysisEnvType

type temp = {
  tables : ParseTablesType.t;
  ambig_table : int Stack.t;
}

let error_goto_entry : goto_entry = 0xffff


let create numTerms numNonterms numStates numProds nontermOrder startState finalProductionIndex =
  assert (Array.length nontermOrder = numNonterms);
  {
    tables = {
      numTerms;
      numNonterms;
      numProds;

      numStates;

      actionCols = numTerms;
      actionTable = Array.make (numTerms * numStates) 0;

      gotoCols = numNonterms;
      gotoTable = Array.make (numNonterms * numStates) 0;

      prodInfo_rhsLen = Array.make numProds 0;
      prodInfo_lhsIndex = Array.make numProds 0;

      stateSymbol = Array.make numStates 0;

      (* table of ambiguous actions is empty until someone fills in the
       * whole thing; since we don't know how many there might be, we
       * can't even allocate the storage now *)
      ambigTable = [||];

      startState = StateId.State.to_int startState;
      finalProductionIndex;

      nontermOrder;
    };
    ambig_table = Stack.create ();
  }

(* -------------- table query -------------- *)
let num_states tables =
  tables.tables.numStates


(* -------------- table construction -------------- *)
let validate_action (code : int) : action_entry =
  (* make sure that 'code' is representable; if this fails, most likely
   * there are more than 32k states or productions; in turn, the most
   * likely cause of *that* would be the grammar is being generated
   * automatically from some other specification *)
  assert (code > -0x7fff && code < 0x7fff);
  code

let validate_goto (code : int) : goto_entry =
  assert (code > -0x7fff && code < 0x7fff);
  code


let append_ambig ambig_table set =
  Stack.push (List.length set) ambig_table;
  List.iter (fun ambig ->
    Stack.push ambig ambig_table
  ) set


let encode_shift tables (dest_state : StateId.State.t) (shifted_term_id : StateId.Terminal.t) : action_entry =
  validate_action (StateId.State.to_int dest_state + 1)

let encode_reduce tables (prod_id : prod_index) (in_state : StateId.State.t) : action_entry =
  validate_action (-prod_id - 1)

let encode_ambig tables (set : action_entry list) (in_state : StateId.State.t) : action_entry =
  let position = Stack.length tables.ambig_table in
  append_ambig tables.ambig_table set;
  validate_action (tables.tables.numStates + position + 1)

let encode_error tables : action_entry =
  validate_action (0)

let encode_goto tables (dest_state : StateId.State.t) (shifted_nonterm_id : StateId.Nonterminal.t) : action_entry =
  validate_goto (StateId.State.to_int dest_state)

let encode_goto_error tables =
  error_goto_entry


let action_entry tables (state_id : StateId.State.t) (term_id : StateId.Terminal.t) =
  tables.tables.actionTable.(StateId.State.to_int state_id * tables.tables.actionCols + StateId.Terminal.to_int term_id)

let set_action_entry tables (state_id : StateId.State.t) (term_id : StateId.Terminal.t) (act : action_entry) =
  tables.tables.actionTable.(StateId.State.to_int state_id * tables.tables.actionCols + StateId.Terminal.to_int term_id) <- act


let goto_entry tables (state_id : StateId.State.t) (nonterm_id : StateId.Nonterminal.t) =
  tables.tables.gotoTable.(StateId.State.to_int state_id * tables.tables.gotoCols + StateId.Nonterminal.to_int nonterm_id)

let set_goto_entry tables (state_id : StateId.State.t) (nonterm_id : StateId.Nonterminal.t) (goto : goto_entry) =
  tables.tables.gotoTable.(StateId.State.to_int state_id * tables.tables.gotoCols + StateId.Nonterminal.to_int nonterm_id) <- goto


let set_state_symbol tables (state_id : StateId.State.t) (sym : symbol_id) =
  tables.tables.stateSymbol.(StateId.State.to_int state_id) <- sym


let set_prod_info tables (prod_id : prod_index) rhsLen (lhsIndex : StateId.Nonterminal.t) =
  tables.tables.prodInfo_rhsLen.(prod_id) <- rhsLen;
  tables.tables.prodInfo_lhsIndex.(prod_id) <- StateId.Nonterminal.to_int lhsIndex


let finish_tables tables =
  let ambigTable = 
    Array.init (Stack.length tables.ambig_table) (fun i ->
      Stack.pop tables.ambig_table
    )
  in
  assert (Stack.is_empty tables.ambig_table);

  BatArray.rev_in_place ambigTable;
  { tables.tables with ambigTable }
