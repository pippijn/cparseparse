(* useract.ml *)
(* interface for user-defined reduction (etc.) actions *)
(* based on elkhound/useract.h *)

(* for now, some actual user actions *)


type functions = {
  reductionActionArray : (SemanticValue.t array -> SemanticValue.t) array;
  duplicateTerminalValueArray : (SemanticValue.t -> SemanticValue.t) array;
  duplicateNontermValueArray : (SemanticValue.t -> SemanticValue.t) array;
  deallocateTerminalValueArray : (SemanticValue.t -> unit) array;
  deallocateNontermValueArray : (SemanticValue.t -> unit) array;
  mergeAlternativeParsesArray : (SemanticValue.t -> SemanticValue.t -> SemanticValue.t) array;
  keepNontermValueArray : (SemanticValue.t -> bool) array;
  reclassifyTokenArray : (SemanticValue.t -> int) array;
}

let default_dup   (sym : int) (sval : SemanticValue.t) : SemanticValue.t = sval
let default_del   (sym : int) (sval : SemanticValue.t) : unit = ()
let default_merge (sym : int) (left : SemanticValue.t) (right : SemanticValue.t) : SemanticValue.t =
  Printf.printf "warning: no function to merge nonterminal %d\n" sym; left
let default_keep  (sym : int) (sval : SemanticValue.t) : bool = true
let default_classify (oldTokenType : int) (sval : SemanticValue.t) : int = oldTokenType


(* collection of actions for use during parsing *)
(* again, see elkhound/useract.h for more info *)
type t = {
  (* action to perform upon performing a reduction *)
  reductionAction :
    (*context?*)
    int ->                     (* production being used to reduce *)
    SemanticValue.t array ->   (* array of svals for RHS symbols *)
    (*loc?*)
    SemanticValue.t;           (* sval for the reduction *)

  (* duplicate a semantic value *)
  duplicateTerminalValue :
    (*context?*)
    int ->                     (* terminal id *)
    SemanticValue.t ->         (* sval being yielded *)
    SemanticValue.t;           (* sval to yield next time *)
  duplicateNontermValue :
    (*context?*)
    int ->                     (* nonterminal id *)
    SemanticValue.t ->         (* sval being yielded *)
    SemanticValue.t;           (* sval to yield next time *)

  (* deallocate an sval that didn't get used *)
  deallocateTerminalValue :
    (*context?*)
    int ->                     (* terminal id *)
    SemanticValue.t ->         (* sval being dropped *)
    unit;
  deallocateNontermValue :
    (*context?*)
    int ->                     (* nonterminal id *)
    SemanticValue.t ->         (* sval being dropped *)
    unit;

  (* merge svals for alternative derivations of the same nonterminal *)
  mergeAlternativeParses :
    int ->                     (* nonterminal with two derivations *)
    SemanticValue.t ->         (* sval from derivation 1 *)  
    SemanticValue.t ->         (* sval from derivation 2 *)
    SemanticValue.t;           (* merged sval *)
    
  (* choose whether to keep or drop a reduced value *)
  keepNontermValue :
    int ->                     (* reduced nonterm id *)
    SemanticValue.t ->         (* sval that 'reductionAction' yielded *)
    bool;                      (* if false, drop the sval on the floor *)
    
  (* reclassification goes here *)
  
  (* debugging support; see useract.h for more info *)
  terminalDescription : int -> SemanticValue.t -> string;
  nonterminalDescription : int -> SemanticValue.t -> string;
  terminalName : int -> string;
  nonterminalName : int -> string;
} 
