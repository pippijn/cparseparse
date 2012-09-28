(* ptreeact.ml *)
(* given actions for a grammar, wrap them with actions that
 * just build a parse tree (forsest) *)
(* based on elkhound/ptreeact *)


open Lexerint       (* tLexerInterface *)
open Parsetables    (* tParseTables *)
open Useract        (* tUserActions *)
open Ptreenode      (* tPTreeNode *)


let inject (n:tPTreeNode) : Obj.t = Obj.repr n
let project (n:Obj.t) : tPTreeNode = Obj.obj n


(* ------------------------ tParseTreeLexer ------------------------- *)
(* wrap the lexer with one yielding parse tree leaf nodes *)
let getToken actions underlying lex =
  (* get next token *)
  underlying lex;
  (* my sval is a tree leaf *)
  lex.sval <- inject (makePTreeNode (actions.terminalName lex.tokType));
  lex


(* ----------------------- parseTreeActions ------------------------ *)
let makeParseTreeActions (underlying: tUserActions) (tables: tParseTables) 
  : tUserActions =
begin
  let actions:tUserActions = {
    (* action to perform upon performing a reduction *)
    reductionAction = (
      fun (prodId:int) (svals: tSemanticValue array) -> (
        (* production info *)
        let rhsLen:int = (getProdInfo_rhsLen tables prodId) in
        let lhsIndex:int = (getProdInfo_lhsIndex tables prodId) in
        
        (* make a tree node, initially with no children *)
        let ret:tPTreeNode = (makePTreeNode (underlying.nonterminalName lhsIndex)) in

        (* add the children *)
        (setNumChildren ret rhsLen);
        for i=0 to rhsLen-1 do
          let child:tPTreeNode = project svals.(i) in
          setChild ret i child;
        done;
        
        inject ret
      ));
      
    (* duplicate a semantic value: trivial *)
    duplicateTerminalValue = (fun _ sval -> sval);
    duplicateNontermValue = (fun _ sval -> sval);

    (* deallocate an sval that didn't get used: trivial *)
    deallocateTerminalValue = (fun _ _ -> ());
    deallocateNontermValue = (fun _ _ -> ());

    (* merge svals for alternative derivations of the same nonterminal *)
    mergeAlternativeParses = (
      fun (ntIndex:int) (left:tSemanticValue) (right:tSemanticValue) -> (
        let l:tPTreeNode = project left in
        let r:tPTreeNode = project right in
        
        addAlternative l r;
        inject l
      ));

    (* choose whether to keep or drop a reduced value: trivial *)
    keepNontermValue = (fun _ _ -> true);

    terminalDescription = (fun id _ -> (underlying.terminalName id));
    nonterminalDescription = (fun id _ -> (underlying.nonterminalName id));
    terminalName = (fun id -> (underlying.terminalName id));
    nonterminalName = (fun id -> (underlying.nonterminalName id));
  } in
  
  actions
end


(* EOF *)
